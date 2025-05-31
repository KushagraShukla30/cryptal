
![Logo](https://github.com/KushagraShukla30/cryptal/blob/main/rsconnect/shinyapps.io/cryptal/cryptalfullgh.png)


# Cryptal

Cryptal is a powerful, open-source web application built with R Shiny that provides detailed insights into cryptocurrency market data. It allows users to explore price trends, analyze fundamentals, perform technical analysis, and receive AI-powered investment suggestions.

# Live Deployment

Try the app live: cryptal.shinyapps.io/cryptalCheck it here: ![(https://cryptal.shinyapps.io/cryptal/)](https://cryptal.shinyapps.io/cryptal/)


## Features

- Live Market Data : Get up-to-date prices, volume, and market caps for major cryptocurrencies.
- Fundamental Analysis : Evaluate projects based on market cap, trading volume, development activity, and community engagement.
- Technical Analysis : View moving averages, volatility metrics, support/resistance levels, and price trends.
- AI Suggestions : Receive investment guidance based on combined fundamental and technical scores.
- Search & Filter : Easily find any cryptocurrency by name, symbol, or ID.
- Interactive Charts : Visualize historical price movements using Plotly.


## Technology used

- **R Shiny** – Web framework
- **CoinGecko API** – Data source
- **DT** – Interactive data tables
- **Plotly** – Charting library
- **Shiny Themes** – UI stylingmarkdown
## Run Locally

1. Clone the repository:
```bash
git clone https://github.com/yourusername/cryptal.git
cd cryptal
```
- Open RStudio and:

- Click "File" > "Open Project" and select the cryptal folder

- Open app.R file

- Click "Run App" button in the top-right of the script editor



Alternatively, you can run directly from R console:

```R
shiny::runApp("enter your path to cryptal folder")
```
## Demo

![Demo](https://github.com/KushagraShukla30/cryptal/blob/main/rsconnect/shinyapps.io/cryptal/cryptal.gif)
