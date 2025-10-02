# ====================================================================
# Virginia Soybean Yield Forecast Shiny App (Forecast vs Actual)
# ====================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readxl)

# ------------------ Load Data from Excel -----------------------------
data_file <- "soybean_yield_forecast_data.xlsx"

annual <- read_excel(data_file, sheet = "Annual_2014_2024")
weekly_raw <- read_excel(data_file, sheet = "Weekly_Raw")
annual_yield <- read_excel(data_file, sheet = "Annual_Yield")

# Standardize types
annual <- annual %>%
  mutate(
    Year = as.integer(Year),
    Yield = as.numeric(Yield),
    Trend_Yield = as.numeric(Trend_Yield),
    Percent_Deviation = as.numeric(Percent_Deviation)
  )

weekly_raw <- weekly_raw %>%
  mutate(
    Year = as.integer(Year),
    Week = as.Date(Week)
  )

# Regression model trained on 2014–2024
reg_model_soy <- lm(Percent_Deviation ~ Excellent + Good + Fair + Poor, data = annual)

# ------------------ Forecast Function -------------------------------
make_forecasts <- function(year) {
  conds <- weekly_raw %>% filter(Year == year)
  if (nrow(conds) == 0) return(NULL)
  
  df <- conds %>%
    rename(
      Excellent = Excellent,
      Good      = Good,
      Fair      = Fair,
      Poor      = Poor,
      VeryPoor  = VeryPoor
    ) %>%
    mutate(
      Forecast_Dev = predict(reg_model_soy, newdata = ., allow.new.levels = TRUE),
      Trend_Yield = predict(lm(Yield ~ Year, data = annual),
                            newdata = data.frame(Year = year)),
      Forecast_Yield = Trend_Yield * (1 + Forecast_Dev / 100)
    )
  
  return(df)
}

# ------------------ Shiny App ----------------------------------------
ui <- fluidPage(
  titlePanel("🌱 Virginia Soybean Yield Forecasts (2014–2024)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year_hist", "Select Year:",
                  choices = sort(unique(annual$Year)),
                  selected = 2024)
    ),
    mainPanel(
      plotlyOutput("histPlot"),
      textOutput("histSummary")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Forecast vs Actual ----
  output$histPlot <- renderPlotly({
    df <- make_forecasts(as.integer(input$year_hist))
    if (is.null(df)) return(NULL)
    
    actual <- annual %>% filter(Year == input$year_hist) %>% pull(Yield)
    
    p <- ggplot(df, aes(x = Week, y = Forecast_Yield)) +
      geom_line(color = "#2E8B57", size = 1.2) +   # forecast (green line)
      geom_point(color = "#228B22") +
      geom_hline(yintercept = actual, color = "#8B0000", linetype = "dashed", size = 1) +
      labs(title = paste("Soybean Yield Forecast vs Actual -", input$year_hist),
           subtitle = "Green = forecasted yield, Red dashed = actual yield",
           x = "Week", y = "Yield (bu/acre)") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  output$histSummary <- renderText({
    df <- make_forecasts(as.integer(input$year_hist))
    if (is.null(df)) return("No data available")
    actual <- annual %>% filter(Year == input$year_hist) %>% pull(Yield)
    rmse <- sqrt(mean((df$Forecast_Yield - actual)^2, na.rm = TRUE))
    paste("RMSE for", input$year_hist, "=", round(rmse, 2), "bu/acre")
  })
}

shinyApp(ui, server)
