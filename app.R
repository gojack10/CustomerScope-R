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

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "CustomerScope Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("users")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
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
}

# Run the application
shinyApp(ui = ui, server = server)
