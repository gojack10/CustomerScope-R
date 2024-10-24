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
            selectInput("cluster", "Select Customer Cluster:",
                       choices = sort(unique(customer_metrics$Cluster)),
                       selected = unique(customer_metrics$Cluster)[1]),
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
  
  # Reactive filtered data
  filtered_data <- reactive({
    customer_metrics %>% filter(Cluster == input$cluster)
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
    ggplot(filtered_data(), aes(x = as.factor(Cluster), y = Monetary, fill = as.factor(Cluster))) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Monetary Value Distribution for Cluster", input$cluster),
           y = "Monetary Value",
           x = "Cluster") +
      scale_y_continuous(labels = scales::dollar_format())
  })
  
  output$frequencyScatter <- renderPlot({
    ggplot(filtered_data(), aes(x = Frequency, y = Monetary)) +
      geom_point(color = "blue", alpha = 0.6) +
      theme_minimal() +
      labs(title = paste("Frequency vs Monetary for Cluster", input$cluster),
           x = "Frequency",
           y = "Monetary Value") +
      scale_y_continuous(labels = scales::dollar_format())
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

  # ... rest of server code ...
}

# Run the application
shinyApp(ui = ui, server = server)
