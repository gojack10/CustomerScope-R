# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)
library(shinydashboard)
library(scales)
library(prophet)

# Source the required scripts to get data
source("marketing/data_loading.R")
source("customer_segmentation.R")
source("marketing/time_series_analysis.R")

# Load churn data
churn_data <- read.csv("data/customer_churn_risk.csv")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "CustomerScope Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("users")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Churn Analysis", tabName = "churn", icon = icon("user-minus")),
      menuItem("Customer Details", tabName = "details", icon = icon("user"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_customers", width = 4),
          valueBoxOutput("avg_monetary", width = 4),
          valueBoxOutput("avg_frequency", width = 4)
        ),
        fluidRow(
          box(plotOutput("segment_distribution"), width = 6),
          box(plotOutput("rfm_distribution"), width = 6)
        )
      ),
      
      # Cluster Analysis Tab
      tabItem(tabName = "cluster",
        fluidRow(
          box(
            selectInput("clusters", "Select Customer Cluster(s):",
                       choices = sort(unique(customer_metrics$Cluster)),
                       selected = sort(unique(customer_metrics$Cluster)[1]),  # Select first cluster by default
                       multiple = TRUE),
            width = 12
          )
        ),
        fluidRow(
          box(plotOutput("monetaryBoxplot"), width = 6),
          box(plotOutput("frequencyScatter"), width = 6)
        ),
        fluidRow(
          box(
            title = "Cluster Profile",
            dataTableOutput("clusterProfile"),
            width = 12
          )
        )
      ),
      
      # Time Series Tab
      tabItem(tabName = "timeseries",
        fluidRow(
          box(plotOutput("sales_trend"), width = 12)
        ),
        fluidRow(
          box(plotOutput("sales_forecast"), width = 12)
        )
      ),
      
      # Churn Analysis Tab
      tabItem(tabName = "churn",
        fluidRow(
          box(width = 12,
            tabsetPanel(
              tabPanel("Recency Analysis",
                plotlyOutput("churn_recency", height = "800px")
              ),
              tabPanel("Risk Heatmap",
                plotlyOutput("churn_heatmap", height = "800px")
              )
            )
          )
        )
      ),
      
      # Customer Details Tab
      tabItem(tabName = "details",
        fluidRow(
          box(
            dataTableOutput("customerTable"),
            width = 12
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output) {
  # Define a fixed color palette for clusters
  cluster_colors <- setNames(
    scales::hue_pal()(length(unique(customer_metrics$Cluster))),
    sort(unique(customer_metrics$Cluster))
  )
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$clusters)  # Ensure clusters are selected
    customer_metrics %>% 
      filter(Cluster %in% input$clusters)
  })
  
  # Overview Tab Outputs
  output$total_customers <- renderValueBox({
    valueBox(
      formatC(nrow(customer_metrics), big.mark = ","),
      "Total Customers",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_monetary <- renderValueBox({
    valueBox(
      paste0("$", formatC(mean(customer_metrics$Monetary), digits = 2, format = "f", big.mark = ",")),
      "Average Customer Value",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_frequency <- renderValueBox({
    valueBox(
      formatC(mean(customer_metrics$Frequency), digits = 1, format = "f"),
      "Average Purchase Frequency",
      icon = icon("shopping-cart"),
      color = "purple"
    )
  })
  
  # Cluster Analysis Tab Outputs
  output$monetaryBoxplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = as.factor(Cluster), y = Monetary, fill = as.factor(Cluster))) +
      geom_boxplot() +
      theme_minimal() +
      labs(
        title = paste("Monetary Value Distribution for Cluster(s):", 
                     paste(sort(input$clusters), collapse = ", ")),
        y = "Monetary Value",
        x = "Cluster"
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(name = "Cluster", values = cluster_colors)  # Use fixed colors
  })
  
  output$frequencyScatter <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Frequency, y = Monetary, color = as.factor(Cluster))) +
      geom_point(alpha = 0.6) +
      theme_minimal() +
      labs(
        title = paste("Frequency vs Monetary for Cluster(s):", 
                     paste(sort(input$clusters), collapse = ", ")),
        x = "Frequency",
        y = "Monetary Value",
        color = "Cluster"
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_color_manual(name = "Cluster", values = cluster_colors)  # Use fixed colors
  })
  
  # Customer Details Tab Output
  output$customerTable <- renderDataTable({
    datatable(customer_metrics,
             options = list(pageLength = 10),
             rownames = FALSE) %>%
      formatCurrency(columns = "Monetary") %>%
      formatRound(columns = c("Frequency", "Recency"), digits = 2)
  })
  
  # Time Series Tab Outputs
  output$sales_trend <- renderPlot({
    ggplot(monthly_sales_clean, aes(x = ds, y = y)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Historical Monthly Sales",
           x = "Month",
           y = "Sales") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")
      )
  })

  output$sales_forecast <- renderPlot({
    forecast_plot_prophet
  })

  # Update clusterProfile
  output$clusterProfile <- renderDataTable({
    req(filtered_data())
    profile_data <- filtered_data() %>%
      group_by(Cluster) %>%
      summarize(
        Total_Customers = n(),
        Avg_Monetary = mean(Monetary, na.rm = TRUE),
        Avg_Frequency = mean(Frequency, na.rm = TRUE),
        Avg_Recency = mean(Recency, na.rm = TRUE)
      ) %>%
      as.data.frame()
    
    datatable(
      profile_data,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      formatCurrency(columns = "Avg_Monetary") %>%
      formatRound(columns = c("Avg_Frequency", "Avg_Recency"), digits = 2)
  })
  
  # Create custom transformations for the heatmap
  y_trans <- scales::trans_new(
    name = "custom_y",
    transform = function(x) ifelse(x <= 2, x * 5, 10),
    inverse = function(x) ifelse(x <= 10, x/5, 2)
  )

  x_trans <- scales::trans_new(
    name = "custom_x",
    transform = function(x) ifelse(x <= 100, x, 100 + (x-100)/3),
    inverse = function(x) ifelse(x <= 100, x, (x-100)*3 + 100)
  )

  # Churn Analysis Tab Outputs
  output$churn_recency <- renderPlotly({
    # Use all data for smoothing
    smooth_fit <- loess(Churn_Probability ~ Recency, 
                       data = churn_data,
                       span = 0.75,  # ggplot2 default
                       degree = 2)   # ggplot2 default
    
    pred_df <- data.frame(
      Recency = seq(min(churn_data$Recency), 
                    max(churn_data$Recency), 
                    length.out = 100)
    )
    pred <- predict(smooth_fit, newdata = pred_df, se = TRUE)
    
    pred_df$fit <- pred$fit
    pred_df$lower <- pred$fit - 1.96 * pred$se.fit
    pred_df$upper <- pred$fit + 1.96 * pred$se.fit
    
    # Filter visible points only (but after smoothing calculation)
    visible_points <- churn_data[churn_data$Churn_Probability > 0, ]
    
    # Create the plot
    p <- plot_ly() %>%
      add_trace(data = visible_points,  # Only show non-zero points
                x = ~Recency, y = ~Churn_Probability, 
                type = 'scatter', mode = 'markers',
                marker = list(color = 'darkblue', opacity = 0.4),
                name = 'Data',
                text = ~paste("Customer ID:", CustomerID,
                            "<br>Monetary Value: $", formatC(Monetary, format="f", digits=2, big.mark=","))) %>%
      add_ribbons(data = pred_df,
                 x = ~Recency, ymin = ~lower, ymax = ~upper,
                 fillcolor = 'rgba(255,0,0,0.2)',
                 line = list(color = 'transparent'),
                 name = 'Confidence Band') %>%
      add_lines(data = pred_df,
               x = ~Recency, y = ~fit,
               line = list(color = 'red'),
               name = 'Trend') %>%
      layout(
        title = "Customer Recency vs Churn Risk",
        xaxis = list(title = "Days Since Last Purchase"),
        yaxis = list(
          title = "Probability of Customer Churning",
          tickformat = ".0%",
          range = c(0, 1.05),
          tickvals = seq(0, 1, 0.25),
          ticktext = c("0%", "25%", "50%", "75%", "100%")
        ),
        height = 800
      )
    
    p
  })
  
  output$churn_heatmap <- renderPlotly({
    p <- ggplot() +
      geom_point(data = churn_data[order(churn_data$Churn_Probability),], 
                 aes(x = Recency, 
                     y = Transaction_Frequency,
                     color = Churn_Probability,
                     text = paste("Customer ID:", CustomerID,
                                "\nMonetary Value: $", formatC(Monetary, format="f", digits=2, big.mark=","))),
                 size = 2,
                 alpha = 0.6) +
      scale_color_gradient2(
        low = "blue",
        mid = "yellow",
        high = "red",
        midpoint = 0.5
      ) +
      scale_y_continuous(
        trans = y_trans,
        breaks = c(0, 1, 2),
        limits = c(0, 2)
      ) +
      scale_x_continuous(
        trans = x_trans,
        breaks = c(0, 50, 100, 200, 300, 400),
        labels = comma
      ) +
      labs(title = "Customer Churn Risk Heatmap",
           subtitle = "Higher risk customers shown in front",
           x = "Days Since Last Purchase",
           y = "Transaction Frequency",
           color = "Churn\nProbability") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
