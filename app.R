# app.R

library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stockSymbol", "Stock Symbol (e.g., NVDA):", value = "NVDA"),
      dateInput("startDate", "Start Date:", value = "2018-01-01"),
      dateInput("endDate", "End Date:", value = Sys.Date()),
      numericInput("forecastDays", "Days to Forecast:", value = 30, min = 7, max = 90),
      actionButton("goButton", "Run Analysis")
    ),
    
    mainPanel(
      div(style = "text-align: center;", h2("Data Driven Stock Market Prediction")),
      
      h4("Key Performance Indicators", style = "margin-top: 20px; font-weight: bold;"),
      fluidRow(
        column(4, wellPanel(textOutput("latestPrice"))),
        column(4, wellPanel(textOutput("meanPrice"))),
        column(4, wellPanel(textOutput("sdPrice")))
      ),
      
      h3("Stock Price Chart"),
      plotOutput("stockPlot"),
      
      h3("Forecast Plot"),
      plotOutput("forecastPlot"),
      
      h3("Model Summary"),
      verbatimTextOutput("modelSummary"),
      
      hr(),
      div(style = "text-align: center; font-weight: bold; font-size: 14px;",
          "Developed by: Saher Thekedar | Samir Abdaljalil | Asmita Shivling Desai")
    )
  )
)

server <- function(input, output) {
  
  stock_data <- eventReactive(input$goButton, {
    getSymbols(input$stockSymbol, src = "yahoo", from = input$startDate, to = input$endDate, auto.assign = FALSE)
  })
  
  output$latestPrice <- renderText({
    req(stock_data())
    latest <- round(as.numeric(last(Ad(stock_data()))), 2)
    paste("Latest Price: $", latest)
  })

  output$meanPrice <- renderText({
    req(stock_data())
    mean30 <- round(mean(tail(Ad(stock_data()), 30)), 2)
    paste("30-Day Avg: $", mean30)
  })

  output$sdPrice <- renderText({
    req(stock_data())
    sd30 <- round(sd(tail(Ad(stock_data()), 30)), 2)
    paste("30-Day Std Dev: $", sd30)
  })
  
  output$stockPlot <- renderPlot({
    data <- stock_data()
    price <- Ad(data)  # Adjusted close price
    chartSeries(price, theme = chartTheme("white"), TA = NULL)
  })
  
  model_fit <- reactive({
    data <- stock_data()
    ts_data <- ts(Ad(data), frequency = 365)
    fit <- auto.arima(ts_data)
    return(fit)
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