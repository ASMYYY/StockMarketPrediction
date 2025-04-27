![Stock Market Prediction Banner](www/header.png)

This Shiny application forecasts NVDA stock prices using time-series models (ARIMAX, Naive) and machine learning models (XGBoost).
The app also includes visual KPIs, model comparisons, forecast breakdowns, and confidence analysis features.

Link: 
- On Shiny Apps: https://asmita-desai.shinyapps.io/stockmarketprediction/
- On Render: https://stockmarketprediction-uqi4.onrender.com/
---

## How to Run Locally

```bash
1. Download or clone the repository, ensuring the following files and folders are included:
   - app.R
   - install.R
   - /www/ folder (must contain header.png)

2. Install required R packages (only needed once):

   install.packages(c(
     "shiny", "shinythemes", "quantmod", "forecast", "ggplot2",
     "tseries", "xgboost", "shinyjs", "fmsb", "tidyr", "scales"
   ))

3. Set your working directory to the project folder if necessary:

   setwd("path/to/your/StockMarketPrediction")

4. Run the application:

   shiny::runApp()

5. Access the app locally in your browser at:

   http://127.0.0.1:xxxx
