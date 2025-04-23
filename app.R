# app.R

library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Data Driven Stock Market Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("startDate", "Start Date:", value = "2018-01-01"),
      numericInput("forecastDays", "Days to Forecast:", value = 30, min = 7, max = 90),
      actionButton("goButton", "Run Analysis")
    ),
    
    mainPanel(
      h3("Stock Price Chart"),
      plotOutput("stockPlot"),
      
      h3("Forecast Plot"),
      plotOutput("forecastPlot"),
      
      h3("Model Summary"),
      verbatimTextOutput("modelSummary")
    )
  )
)

server <- function(input, output) {
  
  stock_data <- eventReactive(input$goButton, {
    getSymbols("NVDA", src = "yahoo", from = input$startDate, auto.assign = FALSE)
  })
  
  output$stockPlot <- renderPlot({
    data <- stock_data()
    price <- Ad(data)  # Adjusted close price
    chartSeries(price, theme = chartTheme("white"), TA = NULL)
  })
  
  model_fit <- reactive({
    data <- stock_data()
    price <- Ad(data)
    ts_data <- ts(price, frequency = 365)
    auto.arima(ts_data)
  })
  
  output$forecastPlot <- renderPlot({
    fit <- model_fit()
    fc <- forecast(fit, h = input$forecastDays)
    plot(fc)
  })
  
  output$modelSummary <- renderPrint({
    tryCatch({
      summary(model_fit())
    }, error = function(e) {
      cat("Error generating summary:\n", e$message)
    })
  })
}

shinyApp(ui = ui, server = server)