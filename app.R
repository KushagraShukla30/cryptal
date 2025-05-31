library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(shinythemes)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)

# Function to fetch top 100 crypto data
get_top_100_crypto_data <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/markets" 
  res <- GET(url, query = list(
    vs_currency = "inr",
    order = "market_cap_desc",
    per_page = 100,
    page = 1,
    sparkline = "false"
  ))
  if (res$status_code != 200) {
    return(data.frame())
  }
  data <- fromJSON(rawToChar(res$content))
  data %>%
    select(id, name, symbol, current_price, market_cap, price_change_percentage_24h, 
           total_volume, circulating_supply, market_cap_rank, ath, atl, 
           ath_change_percentage, atl_change_percentage) %>%
    rename(
      `Coin ID` = id,
      `Name` = name,
      `Symbol` = symbol,
      `Price (₹)` = current_price,
      `Market Cap` = market_cap,
      `24h Change (%)` = price_change_percentage_24h,
      `24h Volume` = total_volume,
      `Circulating Supply` = circulating_supply,
      `Rank` = market_cap_rank,
      `ATH` = ath,
      `ATL` = atl,
      `ATH Change (%)` = ath_change_percentage,
      `ATL Change (%)` = atl_change_percentage
    ) %>%
    mutate(
      `Price (₹)` = round(`Price (₹)`, 2),
      `24h Change (%)` = round(`24h Change (%)`, 2),
      `ATH Change (%)` = round(`ATH Change (%)`, 2),
      `ATL Change (%)` = round(`ATL Change (%)`, 2)
    )
}

# Function to get full coin list
get_full_coin_list <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/list" 
  res <- GET(url)
  if (res$status_code != 200) {
    return(data.frame())
  }
  fromJSON(rawToChar(res$content))
}

# Function to get market data for specific coins
get_coin_market_data <- function(coin_ids) {
  ids_string <- paste(coin_ids, collapse = ",")
  url <- "https://api.coingecko.com/api/v3/coins/markets" 
  res <- GET(url, query = list(
    vs_currency = "inr",
    ids = ids_string,
    order = "market_cap_desc",
    per_page = length(coin_ids),
    page = 1,
    sparkline = "false"
  ))
  if (res$status_code != 200) {
    return(NULL)
  }
  data <- fromJSON(rawToChar(res$content))
  if (length(data) == 0) return(NULL)
  data %>%
    select(id, name, symbol, current_price, market_cap, price_change_percentage_24h,
           total_volume, circulating_supply, market_cap_rank, ath, atl,
           ath_change_percentage, atl_change_percentage) %>%
    rename(
      `Coin ID` = id,
      `Name` = name,
      `Symbol` = symbol,
      `Price (₹)` = current_price,
      `Market Cap` = market_cap,
      `24h Change (%)` = price_change_percentage_24h,
      `24h Volume` = total_volume,
      `Circulating Supply` = circulating_supply,
      `Rank` = market_cap_rank,
      `ATH` = ath,
      `ATL` = atl,
      `ATH Change (%)` = ath_change_percentage,
      `ATL Change (%)` = atl_change_percentage
    ) %>%
    mutate(
      `Price (₹)` = round(`Price (₹)`, 2),
      `24h Change (%)` = round(`24h Change (%)`, 2),
      `ATH Change (%)` = round(`ATH Change (%)`, 2),
      `ATL Change (%)` = round(`ATL Change (%)`, 2)
    )
}

# Function to get detailed info about a coin
get_coin_details <- function(coin_id) {
  url <- paste0("https://api.coingecko.com/api/v3/coins/",  coin_id)
  res <- GET(url, query = list(
    localization = "false",
    tickers = "false",
    market_data = "true",
    community_data = "true",
    developer_data = "true",
    sparkline = "false"
  ))
  if (res$status_code != 200) {
    return(NULL)
  }
  fromJSON(rawToChar(res$content))
}

# Function to get historical price data
get_price_history <- function(coin_id, days = 30) {
  url <- paste0("https://api.coingecko.com/api/v3/coins/",  coin_id, "/market_chart")
  res <- GET(url, query = list(
    vs_currency = "inr",
    days = days,
    interval = if(days <= 1) "hourly" else "daily"
  ))
  if (res$status_code != 200) {
    return(NULL)
  }
  data <- fromJSON(rawToChar(res$content))
  if (is.null(data$prices)) return(NULL)
  data.frame(
    timestamp = as.POSIXct(data$prices[,1]/1000, origin = "1970-01-01"),
    price = data$prices[,2]
  )
}

# Fundamental analysis
analyze_fundamentals <- function(coin_data) {
  if (is.null(coin_data)) return("Unable to analyze fundamentals - no data available")
  
  score <- 0
  factors <- list()
  
  # --- Market Cap ---
  market_cap <- coin_data$market_data$market_cap$inr
  if (!is.null(market_cap)) {
    if (market_cap > 1e9) { # > $1 billion
      score <- score + 25
      factors[[length(factors)+1]] <- "This cryptocurrency has a large market capitalization, indicating strong adoption and institutional interest. This suggests a relatively stable investment."
    } else if (market_cap > 1e8) { # > $100 million
      score <- score + 15
      factors[[length(factors)+1]] <- "The asset has a moderate market cap, indicating some level of adoption but may still be subject to high volatility and manipulation."
    } else {
      score <- score + 5
      factors[[length(factors)+1]] <- "This is a small-cap cryptocurrency with limited liquidity and higher risk. It may offer high reward potential but should only be considered by experienced traders."
    }
  }
  
  # --- Trading Volume ---
  volume_24h <- coin_data$market_data$total_volume$inr
  if (!is.null(volume_24h) && !is.null(market_cap)) {
    vol_ratio <- volume_24h / market_cap
    if (vol_ratio > 0.1) {
      score <- score + 20
      factors[[length(factors)+1]] <- "High trading volume relative to market cap indicates strong investor interest and healthy liquidity. This makes it easier to enter or exit positions without significant price slippage."
    } else if (vol_ratio > 0.03) {
      score <- score + 10
      factors[[length(factors)+1]] <- "Moderate trading volume suggests decent liquidity, but investors should remain cautious as it may not support large trades easily."
    } else {
      score <- score + 3
      factors[[length(factors)+1]] <- "Low trading volume compared to market cap indicates poor liquidity. This increases the risk of price manipulation and difficulty in exiting positions quickly."
    }
  }
  
  # --- Development Activity ---
  if (!is.null(coin_data$developer_data)) {
    commits <- coin_data$developer_data$commit_count_4_weeks %||% 0
    if (commits > 50) {
      score <- score + 20
      factors[[length(factors)+1]] <- "Active development over the past month shows strong commitment from the core team. Frequent updates often lead to better security, new features, and long-term sustainability."
    } else if (commits > 10) {
      score <- score + 10
      factors[[length(factors)+1]] <- "Development activity is ongoing, though at a moderate pace. The project appears maintained but lacks rapid innovation."
    } else {
      score <- score + 2
      factors[[length(factors)+1]] <- "Very little or no recent development activity detected. This could signal lack of interest or abandonment of the project, which increases investment risk."
    }
  }
  
  # --- Community Engagement ---
  if (!is.null(coin_data$community_score)) {
    community_score <- coin_data$community_score
    if (community_score > 70) {
      score <- score + 15
      factors[[length(factors)+1]] <- "Strong community engagement across social media platforms and forums. A passionate user base can drive adoption, provide feedback, and help sustain interest in the project."
    } else if (community_score > 30) {
      score <- score + 8
      factors[[length(factors)+1]] <- "Community presence is visible but not overwhelming. There's room for growth in terms of awareness and grassroots support."
    } else {
      score <- score + 3
      factors[[length(factors)+1]] <- "Minimal community involvement. Lack of public interest reduces the likelihood of widespread adoption and support during critical periods."
    }
  }
  
  # --- Price Position Relative to ATH ---
  ath_change <- coin_data$market_data$ath_change_percentage$inr %||% 0
  if (ath_change > -20) {
    score <- score + 15
    factors[[length(factors)+1]] <- "Currently near all-time high levels, suggesting strong investor confidence and positive momentum. This could indicate a bullish phase, though caution is advised about entering at elevated prices."
  } else if (ath_change > -50) {
    score <- score + 10
    factors[[length(factors)+1]] <- "Trading moderately below all-time highs. The asset may be consolidating or undergoing a correction. Could represent a good buying opportunity depending on other fundamentals."
  } else if (ath_change > -80) {
    score <- score + 5
    factors[[length(factors)+1]] <- "Significantly below all-time highs. This may suggest underlying issues such as loss of confidence, regulatory problems, or failure to deliver on promises."
  } else {
    score <- score + 2
    factors[[length(factors)+1]] <- "Far below all-time highs. Recovery is uncertain and would likely require major improvements or external catalysts. High-risk investment."
  }
  
  # --- Final Risk Assessment ---
  if (score >= 80) {
    risk_level <- "LOW RISK"
    recommendation <- "Based on strong fundamentals including high market cap, solid volume, active development, and growing community, this asset appears to be a safe long-term investment."
  } else if (score >= 60) {
    risk_level <- "MEDIUM RISK"
    recommendation <- "Fundamentals are mixed. Some strengths exist, but there are also areas of concern. Consider investing cautiously and monitor developments closely."
  } else if (score >= 40) {
    risk_level <- "HIGH RISK"
    recommendation <- "Multiple weaknesses in fundamentals suggest caution. The asset may have speculative appeal, but risks outweigh the potential rewards for most investors."
  } else {
    risk_level <- "VERY HIGH RISK"
    recommendation <- "Poor fundamentals across multiple dimensions. Investment is highly speculative and suitable only for those with high risk tolerance and deep understanding of the space."
  }
  
  color <- switch(risk_level,
                  "LOW RISK" = "#059669",
                  "MEDIUM RISK" = "#d97706",
                  "HIGH RISK" = "#f97316",
                  "VERY HIGH RISK" = "#dc2626")
  
  analysis <- paste0(
    "<h4>Fundamental Analysis</h4>",
    "<p><strong>Score:</strong> ", score, "/100</p>",
    "<p><strong>Risk Level:</strong> <span style='color:", color, "'>", risk_level, "</span></p>",
    "<ul>", paste0("<li>", factors, "</li>", collapse = ""), "</ul>",
    "<h5>Recommendation</h5>",
    "<p>", recommendation, "</p>"
  )
  
  return(analysis)
}

# Technical analysis
analyze_technicals <- function(price_data, coin_data) {
  if (is.null(price_data) || nrow(price_data) < 10) {
    return("Insufficient data for technical analysis")
  }
  current_price <- tail(price_data$price, 1)
  prices <- price_data$price
  ma_7 <- mean(tail(prices, min(7, length(prices))))
  ma_30 <- mean(tail(prices, min(30, length(prices))))
  recent_trend <- if (current_price > ma_7) "BULLISH" else "BEARISH"
  long_trend <- if (ma_7 > ma_30) "UPTREND" else "DOWNTREND"
  returns <- diff(log(prices))
  volatility <- sd(returns, na.rm = TRUE) * 100
  vol_assessment <- if (volatility > 10) "HIGH" else if (volatility > 5) "MODERATE" else "LOW"
  support <- min(tail(prices, 30))
  resistance <- max(tail(prices, 30))
  analysis <- paste0(
    "<h4>Technical Analysis</h4>",
    "<p><strong>Current Trend:</strong> <span style='color:", 
    if (recent_trend == "BULLISH") "#059669" else "#dc2626", 
    "'>", recent_trend, "</span></p>",
    "<p><strong>Long-term Trend:</strong> ", long_trend, "</p>",
    "<p><strong>Volatility:</strong> ", vol_assessment, " (", round(volatility, 2), "%)</p>",
    "<p><strong>7-day Average:</strong> ₹", format(ma_7, big.mark = ",", digits = 2), "</p>",
    "<p><strong>30-day Average:</strong> ₹", format(ma_30, big.mark = ",", digits = 2), "</p>",
    "<p><strong>Support Level:</strong> ₹", format(support, big.mark = ",", digits = 2), "</p>",
    "<p><strong>Resistance Level:</strong> ₹", format(resistance, big.mark = ",", digits = 2), "</p>"
  )
  return(analysis)
}

# AI-powered suggestions
generate_ai_suggestion <- function(coin_data, fundamental_score, technical_trend) {
  if (is.null(coin_data)) return("Unable to generate suggestions")
  suggestions <- list()
  
  if (fundamental_score >= 70) {
    suggestions <- append(suggestions, "Suitable for long-term investment (HODL strategy)")
  } else if (fundamental_score >= 40) {
    suggestions <- append(suggestions, "⚖️ Consider for medium-term investment with risk management")
  } else {
    suggestions <- append(suggestions, "⚠️ High-risk investment - only for experienced traders")
  }
  
  if (technical_trend == "BULLISH") {
    suggestions <- append(suggestions, "Technical indicators suggest buying opportunity")
  } else {
    suggestions <- append(suggestions, "Consider waiting for better entry points")
  }
  
  suggestions <- append(suggestions, "Never invest more than you can afford to lose")
  analysis <- paste0(
    "<h4>AI-Powered Investment Suggestions</h4>",
    "<ul>", paste0("<li>", suggestions, "</li>", collapse = ""), "</ul>",
    "<p><em>Disclaimer: This is not financial advice. Cryptocurrency investments are subject to market risks.</em></p>"
  )
  return(analysis)
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Head section: favicon + custom styles
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
        font-size: 16px;
        line-height: 1.6;
        color: #1e293b;
      }
      
      .main-container {
        max-width: 1500px;
        margin: 0 auto;
        padding: 32px 24px;
        min-height: 100vh;
      }
      
      /* Header Section */
      .header-section {
        background: linear-gradient(135deg, #ffffff 0%, #fefefe 100%);
        border-radius: 20px;
        padding: 48px 40px;
        margin-bottom: 32px;
        box-shadow: 
          0 1px 3px rgba(0, 0, 0, 0.05),
          0 4px 16px rgba(0, 0, 0, 0.02);
        border: 1px solid rgba(148, 163, 184, 0.1);
        text-align: center;
        position: relative;
        overflow: hidden;
      }
      
      .header-section::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 2px;
        background: linear-gradient(90deg, transparent, #64748b, transparent);
        opacity: 0.3;
      }
      
      .app-title {
        font-size: 3.5rem;
        font-weight: 800;
        color: #0f172a;
        margin: 0 0 12px 0;
        letter-spacing: -0.05em;
        text-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
      }
      
      .app-subtitle {
        font-size: 1.25rem;
        color: #64748b;
        margin: 0;
        font-weight: 500;
        letter-spacing: 0.025em;
      }
      
      /* Search Section */
      .search-section {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        border-radius: 16px;
        padding: 32px;
        margin-bottom: 32px;
        box-shadow: 
          0 1px 3px rgba(0, 0, 0, 0.05),
          0 8px 24px rgba(0, 0, 0, 0.03);
        border: 1px solid rgba(148, 163, 184, 0.08);
      }
      
      .form-control {
        border: 2px solid #e2e8f0;
        border-radius: 12px;
        padding: 16px 20px;
        font-size: 1.1rem;
        font-weight: 500;
        background: #ffffff;
        transition: all 0.2s ease;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.02);
      }
      
      .form-control:focus {
        border-color: #3b82f6;
        box-shadow: 
          0 0 0 3px rgba(59, 130, 246, 0.1),
          0 2px 8px rgba(0, 0, 0, 0.05);
        outline: none;
      }
      
      .btn {
        border: none;
        border-radius: 12px;
        padding: 16px 28px;
        font-weight: 600;
        font-size: 1.1rem;
        letter-spacing: 0.025em;
        transition: all 0.2s ease;
        text-transform: uppercase;
        cursor: pointer;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #1e40af 0%, #3b82f6 100%);
        color: white;
        box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);
      }
      
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(59, 130, 246, 0.4);
        background: linear-gradient(135deg, #1d4ed8 0%, #2563eb 100%);
      }
      
      .btn-secondary {
        background: linear-gradient(135deg, #6b7280 0%, #9ca3af 100%);
        color: white;
        box-shadow: 0 4px 12px rgba(107, 114, 128, 0.2);
      }
      
      .btn-secondary:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(107, 114, 128, 0.3);
        background: linear-gradient(135deg, #4b5563 0%, #6b7280 100%);
      }
      
      /* Status Messages */
      .status-message {
        margin-top: 20px;
        padding: 16px 24px;
        border-radius: 12px;
        border-left: 4px solid;
        font-weight: 500;
        backdrop-filter: blur(10px);
      }
      
      .status-success {
        background: rgba(240, 253, 244, 0.8);
        border-left-color: #10b981;
        color: #065f46;
      }
      
      .status-error {
        background: rgba(254, 242, 242, 0.8);
        border-left-color: #ef4444;
        color: #991b1b;
      }
      
      .status-warning {
        background: rgba(255, 251, 235, 0.8);
        border-left-color: #f59e0b;
        color: #92400e;
      }
      
      /* Analysis Section */
      .analysis-section {
        background: rgba(255, 255, 255, 0.98);
        backdrop-filter: blur(10px);
        border-radius: 20px;
        padding: 40px;
        margin-bottom: 32px;
        box-shadow: 
          0 4px 16px rgba(0, 0, 0, 0.04),
          0 1px 3px rgba(0, 0, 0, 0.06);
        border: 1px solid rgba(148, 163, 184, 0.08);
      }
      
      .analysis-section h3 {
        font-size: 1.75rem;
        font-weight: 700;
        color: #0f172a;
        margin-bottom: 24px;
        letter-spacing: -0.025em;
      }
      
      /* Tab Navigation */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0;
        margin-bottom: 32px;
      }
      
      .nav-tabs .nav-item .nav-link {
        border: none;
        border-bottom: 3px solid transparent;
        color: #64748b;
        font-weight: 600;
        padding: 16px 24px;
        margin-bottom: -2px;
        border-radius: 0;
        background: none;
        transition: all 0.2s ease;
      }
      
      .nav-tabs .nav-item .nav-link:hover {
        color: #1e40af;
        border-bottom-color: #93c5fd;
        background: rgba(59, 130, 246, 0.02);
      }
      
      .nav-tabs .nav-item .nav-link.active {
        color: #1e40af;
        border-bottom-color: #3b82f6;
        background: rgba(59, 130, 246, 0.05);
      }
      
      /* Table Section */
      .table-section {
        background: rgba(255, 255, 255, 0.98);
        backdrop-filter: blur(10px);
        border-radius: 20px;
        padding: 40px;
        box-shadow: 
          0 4px 16px rgba(0, 0, 0, 0.04),
          0 1px 3px rgba(0, 0, 0, 0.06);
        border: 1px solid rgba(148, 163, 184, 0.08);
        overflow: hidden;
      }
      
      /* DataTable Styling */
      .dataTables_wrapper {
        font-family: inherit;
      }
      
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border: 2px solid #e2e8f0;
        border-radius: 10px;
        padding: 12px 16px;
        margin: 0 12px;
        font-weight: 500;
        background: #ffffff;
        transition: all 0.2s ease;
      }
      
      .dataTables_wrapper .dataTables_filter input:focus,
      .dataTables_wrapper .dataTables_length select:focus {
        border-color: #3b82f6;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
        outline: none;
      }
      
      .table {
        font-size: 1.05rem;
        margin: 0;
      }
      
      .table thead th {
        background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
        border-bottom: 2px solid #cbd5e1;
        color: #374151;
        font-weight: 700;
        padding: 20px 16px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        font-size: 0.85rem;
        white-space: nowrap;
      }
      
      .table tbody td {
        padding: 20px 16px;
        border-bottom: 1px solid #f1f5f9;
        vertical-align: middle;
        font-weight: 500;
        transition: background-color 0.2s ease;
      }
      
      .table tbody tr:hover {
        background-color: rgba(59, 130, 246, 0.02);
      }
      
      /* Value Change Colors */
      .positive-change {
        color: #059669;
        font-weight: 700;
      }
      
      .negative-change {
        color: #dc2626;
        font-weight: 700;
      }
      
      .neutral-change {
        color: #6b7280;
        font-weight: 600;
      }
      
      /* Loading Overlay */
      .loading-overlay {
        position: fixed;
        top: 0; left: 0;
        width: 100%; height: 100%;
        background: rgba(15, 23, 42, 0.8);
        backdrop-filter: blur(4px);
        display: flex;
        justify-content: center;
        align-items: center;
        z-index: 9999;
      }
      
      .spinner {
        width: 60px;
        height: 60px;
        border: 4px solid rgba(255, 255, 255, 0.1);
        border-top: 4px solid #3b82f6;
        border-radius: 50%;
        animation: spin 1s cubic-bezier(0.68, -0.55, 0.265, 1.55) infinite;
      }
      
      @keyframes spin { 
        0% { transform: rotate(0deg); } 
        100% { transform: rotate(360deg); } 
      }
      
      /* Chart Container */
      .chart-container {
        height: 450px;
        margin: 24px 0;
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .main-container {
          padding: 16px;
        }
        
        .header-section {
          padding: 32px 24px;
        }
        
        .app-title {
          font-size: 2.5rem;
        }
        
        .search-section,
        .analysis-section,
        .table-section {
          padding: 24px;
        }
        
        .btn {
          padding: 14px 20px;
          margin-bottom: 8px;
        }
      }
      
      /* Scrollbar Styling */
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: #f1f5f9;
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: #cbd5e1;
        border-radius: 4px;
        transition: background 0.2s ease;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: #94a3b8;
      }
    "))
  ),
  
  div(class = "main-container",
      
      # Header
      div(class = "header-section",
          h1("Cryptal", class = "app-title"),
          p("Professional Cryptocurrency Analysis & Market Intelligence", class = "app-subtitle")
      ),
      
      # Search Bar
      div(class = "search-section",
          fluidRow(
            column(8,
                   textInput("search_term", NULL, 
                             placeholder = "Search cryptocurrencies (e.g., Bitcoin, ETH, Cardano...)",
                             width = "100%")),
            column(4,
                   actionButton("search_btn", "Search", 
                                class = "btn-primary btn-block", 
                                style = "margin-bottom: 8px;"),
                   actionButton("reset_btn", "Top 100", 
                                class = "btn-secondary btn-block")
            )
          ),
          uiOutput("search_msg")
      ),
      
      # Conditional Analysis Panel
      conditionalPanel(
        condition = "output.show_analysis",
        div(class = "analysis-section",
            h3("Market Analysis & Insights"),
            tabsetPanel(id = "analysis_tabs", type = "tabs",
                        tabPanel("Price Chart", 
                                 div(class = "chart-container",
                                     plotlyOutput("price_chart", height = "100%"))),
                        tabPanel("Fundamental Analysis", 
                                 div(style = "padding: 16px 0;",
                                     htmlOutput("fundamental_analysis"))),
                        tabPanel("Technical Analysis", 
                                 div(style = "padding: 16px 0;",
                                     htmlOutput("technical_analysis"))),
                        tabPanel("AI Insights", 
                                 div(style = "padding: 16px 0;",
                                     htmlOutput("ai_suggestions")))
            )
        )
      ),
      
      # Data Table
      div(class = "table-section",
          DTOutput("crypto_table")
      )
  ),
  
  # Loading Overlay
  conditionalPanel(
    condition = "output.loading",
    div(class = "loading-overlay",
        div(class = "spinner")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  showNotification("Loading cryptocurrency data...", type = "message", duration = 3)
  
  displayed_data <- reactiveVal()
  search_message <- reactiveVal("")
  message_type <- reactiveVal("info")
  selected_coin <- reactiveVal(NULL)
  loading_state <- reactiveVal(FALSE)
  
  top100_static <- reactive({
    data <- get_top_100_crypto_data()
    if(nrow(data) == 0) {
      showNotification("Failed to load data. Please refresh the page.", type = "error", duration = 10)
    }
    data
  })
  
  coin_list_static <- reactive({
    get_full_coin_list()
  })
  
  observe({
    data <- top100_static()
    displayed_data(data)
    if(nrow(data) > 0) {
      search_message("Showing top 100 cryptocurrencies by market cap • Click any row for detailed analysis")
      message_type("info")
    }
  })
  
  observeEvent(input$search_btn, {
    term <- tolower(trimws(input$search_term))
    if (term == "") {
      search_message("Please enter a search term")
      message_type("warning")
      return()
    }
    
    search_message("Searching...")
    message_type("info")
    
    top100_data <- top100_static()
    filtered_top100 <- top100_data %>%
      filter(grepl(term, tolower(Name), fixed = TRUE) | grepl(term, tolower(Symbol), fixed = TRUE) | grepl(term, tolower(`Coin ID`), fixed = TRUE))
    
    if (nrow(filtered_top100) > 0) {
      displayed_data(filtered_top100)
      search_message(paste("Found", nrow(filtered_top100), "cryptocurrency(s) in Top 100 • Click any row for detailed analysis"))
      message_type("success")
      return()
    }
    
    coin_list <- coin_list_static()
    coin_matches <- coin_list %>%
      filter(grepl(term, tolower(name), fixed = TRUE) | grepl(term, tolower(symbol), fixed = TRUE) | grepl(term, tolower(id), fixed = TRUE))
    
    if (nrow(coin_matches) == 0) {
      search_message("No cryptocurrency found. Try different keywords.")
      message_type("error")
      displayed_data(data.frame())
      return()
    }
    
    coin_ids <- head(coin_matches$id, 10)
    coin_data <- get_coin_market_data(coin_ids)
    if (is.null(coin_data) || nrow(coin_data) == 0) {
      search_message("Cryptocurrency found but market data unavailable")
      message_type("warning")
      displayed_data(data.frame())
      return()
    }
    
    displayed_data(coin_data)
    if(nrow(coin_data) == 1) {
      search_message(paste0("Found: ", coin_data$Name[1], " (", coin_data$Symbol[1], ") • Click the row for detailed analysis"))
    } else {
      search_message(paste0("Found ", nrow(coin_data), " cryptocurrencies • Click any row for detailed analysis"))
    }
    message_type("success")
  })
  
  observeEvent(input$reset_btn, {
    displayed_data(top100_static())
    updateTextInput(session, "search_term", value = "")
    search_message("Showing top 100 cryptocurrencies by market cap • Click any row for detailed analysis")
    message_type("info")
    selected_coin(NULL)
  })
  
  observeEvent(input$crypto_table_rows_selected, {
    if (length(input$crypto_table_rows_selected) > 0) {
      row_idx <- input$crypto_table_rows_selected[1]
      data <- displayed_data()
      if (!is.null(data) && nrow(data) >= row_idx) {
        coin_id <- data[row_idx, "Coin ID"]
        selected_coin(coin_id)
        loading_state(TRUE)
        showNotification(paste("Loading detailed analysis for", data[row_idx, "Name"]), type = "message", duration = 3)
      }
    }
  })
  
  observeEvent(selected_coin(), {
    if (!is.null(selected_coin())) {
      coin_id <- selected_coin()
      coin_details <- get_coin_details(coin_id)
      price_history <- get_price_history(coin_id, 30)
      if (!is.null(coin_details)) {
        fund_analysis <- analyze_fundamentals(coin_details)
        tech_analysis <- analyze_technicals(price_history, coin_details)
        trend <- if (grepl("BULLISH", tech_analysis)) "BULLISH" else "BEARISH"
        score <- as.numeric(gsub(".*Score: (\\d+)/100.*", "\\1", fund_analysis))
        if (is.na(score)) score <- 50
        ai_suggestions <- generate_ai_suggestion(coin_details, score, trend)
        output$fundamental_analysis <- renderText({ fund_analysis })
        output$technical_analysis <- renderText({ tech_analysis })
        output$ai_suggestions <- renderText({ ai_suggestions })
        if (!is.null(price_history)) {
          output$price_chart <- renderPlotly({
            plot_ly(price_history, x = ~timestamp, y = ~price, type = 'scatter', mode = 'lines',
                    line = list(color = '#4299e1', width = 2)) %>%
              layout(
                title = paste("Price History -", coin_details$name),
                xaxis = list(title = "Date"),
                yaxis = list(title = "Price (₹)"),
                hovermode = 'x unified',
                plot_bgcolor = 'rgba(0,0,0,0)',
                paper_bgcolor = 'rgba(0,0,0,0)'
              )
          })
        }
      }
      loading_state(FALSE)
    }
  })
  
  output$show_analysis <- reactive({
    !is.null(selected_coin())
  })
  outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)
  
  output$search_msg <- renderUI({
    msg <- search_message()
    if (msg == "") return(NULL)
    div(class = paste("status-message", paste0("status-", message_type())),
        icon = switch(message_type(),
                      "success" = icon("check-circle"),
                      "error" = icon("exclamation-circle"),
                      "warning" = icon("exclamation-triangle"),
                      icon("info-circle")),
        msg
    )
  })
  
  output$loading <- reactive({
    loading_state()
  })
  outputOptions(output, "loading", suspendWhenHidden = FALSE)
  
  output$crypto_table <- renderDT({
    data <- displayed_data()
    if (is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }
    datatable(
      data,
      selection = 'single',
      rownames = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        deferRender = TRUE,
        scrollY = 600,
        scroller = TRUE,
        columnDefs = list(
          list(
            targets = c(4, 5, 6, 7, 9, 10, 11, 12),
            render = JS(
              "function(data, type, row) {
                if (type === 'display') {
                  if (data === null || data === undefined) return '';
                  // Format numbers with commas
                  if (!isNaN(parseFloat(data)) && [4, 6, 7, 9, 10].indexOf(parseInt(this.col)) >= 0) {
                    return parseFloat(data).toLocaleString('en-IN', {maximumFractionDigits: 2, minimumFractionDigits: 2});
                  }
                  // Format percentages with colors
                  if ([5, 11, 12].indexOf(parseInt(this.col)) >= 0) {
                    const value = parseFloat(data);
                    const color = value > 0 ? '#059669' : value < 0 ? '#dc2626' : '#6b7280';
                    return '<span style=\"color:' + color + '; font-weight:600\">' + value.toFixed(2) + '%</span>';
                  }
                }
                return data;
              }"
            )
          ),
          list(targets = 3, className = 'dt-right'),
          list(targets = 2, className = 'dt-center')
        ),
        initComplete = JS(
          "function(settings, json) {
            $(this.api().table().container()).addClass('table-striped');
          }"
        )
      )
    ) %>%
      formatCurrency(columns = c("Price (₹)", "Market Cap", "24h Volume", "ATH", "ATL"), currency = "₹", digits = 2) %>%
      formatStyle(
        '24h Change (%)',
        backgroundColor = styleInterval(c(-0.0001, 0.0001), c('#fee2e2', '#fef3c7', '#dcfce7'))
      )
  })
}

# Run app
shinyApp(ui, server)