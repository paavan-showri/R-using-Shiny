# Load required libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Load transaction data from CSV
transactions_data <- read_csv("transactions.csv")

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Transaction Metrics Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dimension", "Select Dimension:",
                  choices = c("pmt", "pg", "mid", "sub_type"),
                  selected = "pg"),
      sliderInput("threshold", "Success Rate Threshold:",
                  min = 0, max = 100, value = 90)
    ),
    mainPanel(
      plotOutput("success_plot"),
      plotOutput("transactions_plot"),
      plotOutput("line_chart_dimension")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculate success rate and total transactions across dimensions
  dimension_data <- reactive({
    transactions_data %>%
      group_by_at(input$dimension) %>%
      summarise(success_rate = sum(success) / sum(t) * 100,
                total_transactions = sum(t))
  })
  
  # Calculate success rate by 'hr' and grouped by input dimension
  success_rate_by_hr_dimension <- reactive({
    transactions_data %>%
      mutate(hr = as.POSIXct(hr)) %>%
      group_by(hr, across(all_of(input$dimension))) %>%
      summarise(success_rate = sum(success) / sum(t) * 100)
  })
  
  # Filter data based on success rate threshold
  filtered_data <- reactive({
    dimension_data() %>%
      filter(success_rate >= input$threshold)
  })
  
  # Render success rate plot
  output$success_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$dimension, y = "success_rate")) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = input$dimension, y = "Success Rate (%)", title = "Success Rate Across Dimensions") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render total transactions plot
  output$transactions_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$dimension, y = "total_transactions")) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(x = input$dimension, y = "Total Transactions", title = "Total Transactions Across Dimensions") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render line chart for success rate by 'hr' and grouped by input dimension
  output$line_chart_dimension <- renderPlot({
    ggplot(success_rate_by_hr_dimension(), aes(x = hr, y = success_rate, color = !!as.symbol(input$dimension))) +
      geom_line() +
      labs(x = "Hour", y = "Success Rate (%)", title = "Success Rate Over Time by Dimension") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui, server)
