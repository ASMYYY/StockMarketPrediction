# ---- Dependency Installer ----
required_packages <- c("shiny", "shinythemes", "quantmod", "forecast", "ggplot2", "tseries", "xgboost", "shinyjs")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# app.R

library(shiny)
library(shinythemes)
library(quantmod)
library(forecast)
library(ggplot2)
library(tseries)
library(xgboost)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  
  tags$img(src = "header.png", style = "width: 100%; margin-bottom: 20px;"),
  tags$hr(style = "margin-top: -10px; margin-bottom: 20px; border-top: 2px solid #007BFF;"),
  
  fluidRow(
    column(
      width = 9,
      h4("Key Performance Indicators", style = "margin-top: 20px; font-weight: bold;"),
      fluidRow(
        column(4, wellPanel(style = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); padding: 15px; background-color: #fff; border-radius: 6px;", textOutput("latestPrice"))),
        column(4, wellPanel(style = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); padding: 15px; background-color: #fff; border-radius: 6px;", textOutput("meanPrice"))),
        column(4, wellPanel(style = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); padding: 15px; background-color: #fff; border-radius: 6px;", textOutput("sdPrice")))
      ),
      h3("Stock Price Chart"),
      wellPanel(style = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); padding: 15px; background-color: #fff; border-radius: 6px;", plotOutput("stockPlot")),
      h3("Forecast Plot"),
      wellPanel(style = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); padding: 15px; background-color: #fff; border-radius: 6px;", plotOutput("forecastPlot")),
      p("This chart compares the predicted stock prices using three models:",
        strong("ARIMAX (blue)"), ", ", strong("Naive (green)"), ", and ", strong("XGBoost (red)"), "."),
      h3("Model Summary"),
      verbatimTextOutput("modelSummary"),
      h3("Model Comparison"),
      verbatimTextOutput("modelCompare")
    ),
    column(
      width = 3,
      wellPanel(
        textInput("stockSymbol", "Stock Symbol (e.g., NVDA):", value = "NVDA"),
        dateInput("startDate", "Start Date:", value = "2018-01-01"),
        dateInput("endDate", "End Date:", value = Sys.Date()),
        numericInput("forecastDays", "Days to Forecast:", value = 30, min = 7, max = 90),
        actionButton("goButton", "Apply Filters")
      )
    )
  )
)

server <- function(input, output) {
  
  observe({
    shinyjs::click("goButton")
  })
  
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
    price <- Ad(data)
    volume <- Vo(data)
    ts_data <- ts(price, frequency = 365)
    xreg <- as.numeric(volume)
    
    arimax_model <- auto.arima(ts_data, xreg = xreg)
    xreg_future <- rep(mean(xreg, na.rm = TRUE), input$forecastDays)
    arimax_forecast <- forecast(arimax_model, xreg = xreg_future, h = input$forecastDays)
    
    naive_model <- naive(ts_data, h = input$forecastDays)
    
    # XGBoost part
    df <- data.frame(
      y = as.numeric(price),
      lag1 = stats::lag(as.numeric(price), -1),
      lag2 = stats::lag(as.numeric(price), -2),
      vol = as.numeric(volume)
    )
    df <- na.omit(df)
    train_idx <- 1:(nrow(df) - input$forecastDays)
    test_idx <- (nrow(df) - input$forecastDays + 1):nrow(df)
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(df[train_idx, c("lag1", "lag2", "vol")]), label = df$y[train_idx])
    model_xgb <- xgboost::xgboost(data = dtrain, nrounds = 50, verbose = 0)
    dtest <- xgboost::xgb.DMatrix(as.matrix(df[test_idx, c("lag1", "lag2", "vol")]))
    xgb_pred <- predict(model_xgb, dtest)
    
    list(
      arimax = arimax_forecast,
      naive = naive_model,
      xgb = ts(xgb_pred, start = end(ts_data)[1] + 1, frequency = 365),
      actual = ts_data
    )
  })
  
  output$forecastPlot <- renderPlot({
    fits <- model_fit()
    autoplot(fits$arimax, series = "ARIMAX Forecast") +
      autolayer(fits$naive$mean, series = "Naive Forecast", PI = FALSE) +
      autolayer(fits$xgb, series = "XGBoost Forecast", PI = FALSE) +
      labs(
        title = "Forecast Comparison: ARIMAX vs Naive vs XGBoost",
        subtitle = "Each line shows predicted values for the selected forecast period",
        x = "Days Ahead",
        y = "Price ($)",
        color = "Forecast Model"
      ) +
      scale_color_manual(values = c("ARIMAX Forecast" = "#800000", "Naive Forecast" = "#A0522D", "XGBoost Forecast" = "#CD5C5C")) +
      theme_minimal(base_family = "Helvetica") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
  })
  
  output$modelSummary <- renderPrint({
    tryCatch({
      fit <- model_fit()$arimax$model
      cat("Model Summary (ARIMAX):\n\n")
      print(summary(fit))
      cat("\nInterpretation:\n")
      cat("- This ARIMAX model uses Adjusted Close as the main variable and Volume as an external regressor.\n")
      cat("- A good model has low residual autocorrelation and a low AIC.\n")
      cat("- Use residual plots and RMSE/MAPE to judge performance beyond summary stats.\n")
    }, error = function(e) {
      cat("Error generating summary:\n", e$message)
    })
  })
  
  output$modelCompare <- renderPrint({
    fits <- model_fit()
    actual_tail <- tail(fits$actual, input$forecastDays)
    n <- min(length(actual_tail), length(fits$arimax$mean), length(fits$naive$mean), length(fits$xgb))
    arimax_rmse <- sqrt(mean((actual_tail[1:n] - fits$arimax$mean[1:n])^2, na.rm = TRUE))
    naive_rmse <- sqrt(mean((actual_tail[1:n] - fits$naive$mean[1:n])^2, na.rm = TRUE))
    xgb_rmse <- sqrt(mean((actual_tail[1:n] - fits$xgb[1:n])^2, na.rm = TRUE))
    
    cat("Model Comparison:\n")
    cat(sprintf("ARIMAX RMSE: %.2f\n", arimax_rmse))
    cat(sprintf("Naive RMSE: %.2f\n", naive_rmse))
    cat(sprintf("XGBoost RMSE: %.2f\n", xgb_rmse))
    
    cat("\nInterpretation:\n")
    best_rmse <- min(c(arimax_rmse, naive_rmse, xgb_rmse))
    if (best_rmse == arimax_rmse) {
      cat("- ARIMAX performed best, capturing trend and volume influence effectively.\n")
    } else if (best_rmse == xgb_rmse) {
      cat("- XGBoost performed best, leveraging lag features and volume for improved forecasts.\n")
    } else {
      cat("- Naive model did surprisingly well, likely due to strong autocorrelation.\n")
    }
  })
}

shinyApp(ui = ui, server = server)