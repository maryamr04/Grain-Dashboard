# ====================================================================
# Soybean Crop Conditions App
# ====================================================================
library(shiny)
library(plotly)
library(dplyr)
library(rnassqs)

# 🔑 Make sure you have your API key set before running
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# --- Helper: Pull soybean condition data from NASS API ---
get_soybean_conditions <- function(year, state = "VIRGINIA") {
  tryCatch({
    df <- nassqs(list(
      commodity_desc    = "SOYBEANS",
      year              = year,
      state_name        = state,
      statisticcat_desc = "CONDITION",
      agg_level_desc    = "STATE"
    ))
    
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      mutate(
        week = as.Date(week_ending, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
        value = as.numeric(Value),
        condition = gsub("SOYBEANS - CONDITION, MEASURED IN PCT ", "", short_desc)
      ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
  }, error = function(e) NULL)
}

# ====================================================================
# UI
# ====================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
      body { background-color: #F3E5D0 !important; }

      .title-box {
        background:#4B2E2B; color:#FDFBF7;
        padding:25px; text-align:center;
        margin-bottom:20px; border-bottom:3px solid #3E2C23;
        box-shadow:0 4px 10px rgba(0,0,0,0.25);
      }
      .title-box h1 {
        margin:0; font-size:40px;
        font-family:'Times New Roman', serif; font-weight:700;
      }
      .title-box h4 {
        margin-top:6px; font-size:18px;
        font-family:'Times New Roman', serif; font-style:italic;
        color:#EADBC8; font-weight:400;
      }

      /* Tab titles */
      .nav-tabs > li > a {
        color: #FDFBF7 !important;
        font-weight: bold;
        font-family: 'Times New Roman', serif;
      }
      .nav-tabs > li.active > a {
        color: #FDFBF7 !important;
        background-color: #4B2E2B !important;
        border: 1px solid #3E2C23 !important;
      }

      /* Tab content panels */
      .tab-content > .tab-pane {
        background-color:#4B2E2B; color:#FDFBF7;
        border:1px solid #3E2C23; border-radius:6px;
        padding:20px; margin-top:15px;
        box-shadow:0 3px 8px rgba(0,0,0,0.2);
      }
  "))),
  
  # --- Title ---
  div(class = "title-box",
      h1("🌾 Soybean Crop Conditions Dashboard"),
      h4("USDA NASS API · Virginia · 2020–2025")
  ),
  
  # --- About Box ---
  div(style = "
      background-color:#4B2E2B;
      color:#FDFBF7;
      font-family:'Times New Roman', serif;
      padding:15px;
      border-radius:8px;
      margin-bottom:20px;
      box-shadow:0 3px 8px rgba(0,0,0,0.2);",
      h4("About this Data"),
      p("Soybean crop conditions (2020–2025) are pulled directly from the USDA NASS API. 
        Categories include Very Poor, Poor, Fair, Good, and Excellent. 
        Percentages represent the share of soybean acres in each condition.")
  ),
  
  # --- Tabs by Year ---
  do.call(tabsetPanel, c(
    id = "soy_condition_tabs",
    lapply(2020:2025, function(yr) {
      tabPanel(
        title = paste(yr),
        plotlyOutput(paste0("soy_conditions_", yr), height = "400px")
      )
    })
  ))
)

# ====================================================================
# SERVER
# ====================================================================
server <- function(input, output, session) {
  for (yr in 2020:2025) {
    local({
      year_inner <- yr
      output_id <- paste0("soy_conditions_", yr)
      
      output[[output_id]] <- renderPlotly({
        df <- get_soybean_conditions(year_inner)
        
        if (is.null(df) || nrow(df) == 0) {
          return(plotly_empty(type = "scatter", mode = "lines") %>%
                   layout(title = list(text = paste("No data for Soybean Conditions —", year_inner))))
        }
        
        df$condition <- factor(df$condition,
                               levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT"))
        
        plot_ly(
          data = df %>% arrange(week, condition),
          x = ~week,
          y = ~value,
          color = ~condition,
          colors = c(
            "VERY POOR" = "#6D4C41",   # soil brown
            "POOR" = "#D95F02",        # earthy orange
            "FAIR" = "#FFD54F",        # sunlight yellow
            "GOOD" = "#66BB6A",        # healthy green
            "EXCELLENT" = "#1B5E20"    # dark forest green
          ),
          type = "scatter",
          mode = "none",
          stackgroup = "one",
          fill = "tonexty",
          text = ~paste0(
            "<b>Week:</b> ", format(week, "%b %d, %Y"),
            "<br><b>Condition:</b> ", condition,
            "<br><b>Percent:</b> ", value, "%"
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            title = list(text = paste("Soybean Conditions in", year_inner)),
            xaxis = list(title = "Month", tickformat = "%b"),
            yaxis = list(title = "Percent", range = c(0, 100)),
            legend = list(title = list(text = "Condition")),
            plot_bgcolor = "#F3E5D0",
            paper_bgcolor = "#F3E5D0"
          )
      })
    })
  }
}

# ====================================================================
# Run App
# ====================================================================
shinyApp(ui, server)
