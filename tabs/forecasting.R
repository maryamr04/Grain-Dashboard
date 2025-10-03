# ============================================================
# Mini Shiny App: Soybean Yield Forecast (ARIMAX Model)
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)
library(readxl)

# ---- Load Data ----
data_file <- "soybean_yield_forecast_data.xlsx"

soy_annual <- read_excel(data_file, sheet = "Annual_2014_2024") %>%
  mutate(
    Year = as.integer(Year),
    Yield = as.numeric(Yield),
    mean_EDVI = as.numeric(mean_EDVI),
    Excellent = as.numeric(Excellent),
    Good = as.numeric(Good),
    Fair = as.numeric(Fair),
    Poor = as.numeric(Poor),
    VeryPoor = as.numeric(VeryPoor)
  )

# ---- Fit Function ----
make_forecasts_arimax <- function(year) {
  df <- soy_annual %>% filter(Year <= year)
  if (nrow(df) < 5) return(NULL)
  
  y <- ts(df$Yield, start = min(df$Year), frequency = 1)
  
  # Drop one condition to avoid collinearity
  xreg <- df %>% select(mean_EDVI, Excellent, Good, Fair, Poor) %>% as.matrix()
  
  fit <- auto.arima(y, xreg = xreg, seasonal = FALSE)
  
  future_xreg <- soy_annual %>%
    filter(Year == year) %>%
    select(mean_EDVI, Excellent, Good, Fair, Poor) %>% as.matrix()
  
  fcast <- forecast(fit, xreg = future_xreg, h = 1)
  
  tibble(
    Year = df$Year,
    Actual = df$Yield,
    Fitted = fitted(fit),
    Forecast = c(rep(NA, nrow(df)-1), as.numeric(fcast$mean)),
    Lower = c(rep(NA, nrow(df)-1), fcast$lower[,2]),
    Upper = c(rep(NA, nrow(df)-1), fcast$upper[,2])
  )
}


# ---- UI ----
ui <- fluidPage(
  titlePanel("🌾 Soybean Yield Forecast (ARIMAX Model)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year_sel", "Select Forecast Year:",
                  choices = sort(unique(soy_annual$Year)),
                  selected = max(soy_annual$Year))
    ),
    mainPanel(
      plotlyOutput("arimax_plot", height = "600px"),
      br(),
      textOutput("arimax_summary")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  output$arimax_plot <- renderPlotly({
    df_plot <- make_forecasts_arimax(input$year_sel)
    if (is.null(df_plot)) return(NULL)
    
    p <- ggplot(df_plot, aes(x = Year)) +
      geom_line(aes(y = Actual), color = "black", size = 1.2) +
      geom_point(aes(y = Actual), color = "black") +
      geom_line(aes(y = Fitted), color = "blue", linetype = "dashed") +
      geom_line(aes(y = Forecast), color = "green") +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "green") +
      labs(
        title = paste("ARIMAX Forecast up to", input$year_sel),
        subtitle = "Black = Actual | Blue = Fitted | Green = Forecast + CI",
        y = "Yield (bu/acre)", x = "Year"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  output$arimax_summary <- renderText({
    df_plot <- make_forecasts_arimax(input$year_sel)
    if (is.null(df_plot)) return("Not enough data for forecast")
    forecast_val <- tail(df_plot$Forecast, 1)
    paste("ARIMAX Forecast for", input$year_sel, "=", round(forecast_val, 2), "bu/acre")
  })
}

# ---- RUN ----
shinyApp(ui, server)
