# ====================================================================
# Virginia Soybean County Analysis Dashboard (2013–2025)
# ====================================================================
library(shiny)
library(plotly)
library(dplyr)
library(rnassqs)
library(tigris)
library(sf)
library(tidyr)
library(ggplot2)
library(shinycssloaders)

options(tigris_use_cache = TRUE)

# 🔑 NASS API Key
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# --------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------

# Historical (2013–2025) by COUNTY
get_soybean_acres_hist <- function(state = "VA") {
  df <- nassqs(list(
    commodity_desc     = "SOYBEANS",
    state_alpha        = state,
    agg_level_desc     = "COUNTY",
    source_desc        = "SURVEY",
    year               = 2013:2025,
    unit_desc          = "ACRES",
    statisticcat_desc  = c("AREA PLANTED", "AREA HARVESTED")
  ))
  
  if (nrow(df) == 0) return(NULL)
  
  df %>%
    mutate(
      Year = as.numeric(year),
      county_name = tools::toTitleCase(tolower(county_name)),
      Value = as.numeric(gsub(",", "", Value)),
      Var = ifelse(grepl("PLANTED", short_desc), "Planted", "Harvested")
    ) %>%
    select(Year, state_alpha, county_name, Var, Value) %>%
    group_by(Year, state_alpha, county_name, Var) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
    mutate(SuccessRate = ifelse(!is.na(Planted) & Planted > 0,
                                (Harvested / Planted) * 100, NA))
}

# County-level yearly (for maps, 2020–2025)
get_soybean_acres_county <- function(year, state = "VIRGINIA") {
  df <- nassqs(list(
    commodity_desc     = "SOYBEANS",
    state_name         = state,
    agg_level_desc     = "COUNTY",
    source_desc        = "SURVEY",
    year               = year,
    unit_desc          = "ACRES",
    statisticcat_desc  = c("AREA PLANTED", "AREA HARVESTED")
  ))
  
  if (nrow(df) == 0) return(NULL)
  
  df %>%
    mutate(
      County = paste0(toupper(county_name), " COUNTY"),
      Value  = as.numeric(gsub(",", "", Value)),
      Var    = ifelse(grepl("PLANTED", short_desc), "Planted", "Harvested")
    ) %>%
    select(Year = year, County, Var, Value)
}

# Shapefile
va_counties <- counties(state = "VA", cb = TRUE, year = 2021) %>%
  st_as_sf() %>%
  mutate(County = toupper(NAME))

# --------------------------------------------------------------------
# UI
# --------------------------------------------------------------------
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
      margin:0; font-size:38px;
      font-family:'Times New Roman', serif; font-weight:700;
    }
    .title-box h4 {
      margin-top:6px; font-size:18px;
      font-family:'Times New Roman', serif; font-style:italic;
      color:#EADBC8; font-weight:400;
    }
    .about-box {
      background-color:#4B2E2B;
      color:#FDFBF7;
      font-family:'Times New Roman', serif;
      padding:15px;
      border-radius:8px;
      margin-bottom:20px;
      box-shadow:0 3px 8px rgba(0,0,0,0.2);
    }
    h3 { font-family:'Times New Roman', serif; font-weight:700; color:#2E7D32; }
  "))),
  
  div(class = "title-box",
      h1("🌱 Virginia Soybean County Analysis"),
      h4("USDA NASS API · 2013–2025")
  ),
  
  div(class = "about-box",
      h4("About this Data"),
      p("This dashboard uses county-level soybean data from the USDA NASS API (2013–2025).
        Explore acres planted, acres harvested, and harvest success rate across Virginia counties.")
  ),
  
  # --- Historical Line Graphs ---
  h3("🌾 Historical Trends by County (2013–2025)"),
  
  selectInput("county_sel", "Select Counties:", multiple = TRUE,
              choices = NULL, selected = NULL),
  
  h4("Acres Planted"),
  withSpinner(plotlyOutput("plot_planted", height = "350px"),
              type = 4, color = "#FDFBF7", size = 0.7),
  br(),
  
  h4("Acres Harvested"),
  withSpinner(plotlyOutput("plot_harvested", height = "350px"),
              type = 4, color = "#FDFBF7", size = 0.7),
  br(),
  
  h4("Harvest Success Rate (%)"),
  withSpinner(plotlyOutput("plot_success", height = "350px"),
              type = 4, color = "#FDFBF7", size = 0.7),
  
  br(), hr(),
  
  # --- Choropleth Map ---
  h3("🗺️ County-Level Choropleth Map (2020–2025)"),
  
  selectInput("map_year", "Select Year:", choices = 2020:2025, selected = 2020),
  
  selectInput("map_var", "Select Variable:",
              choices = c("Acres Planted" = "Planted",
                          "Acres Harvested" = "Harvested",
                          "Harvest Success Rate (%)" = "SuccessRate"),
              selected = "Planted"),
  
  withSpinner(plotlyOutput("map_soy", height = "500px"),
              type = 4, color = "#FDFBF7", size = 0.7)
)

# --------------------------------------------------------------------
# SERVER
# --------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Load historical once
  soy_hist <- get_soybean_acres_hist("VA")
  
  updateSelectInput(session, "county_sel",
                    choices = sort(unique(soy_hist$county_name)),
                    selected = head(sort(unique(soy_hist$county_name)), 3))
  
  # Historical plots
  output$plot_planted <- renderPlotly({
    req(input$county_sel)
    df <- soy_hist %>% filter(county_name %in% input$county_sel)
    gg <- ggplot(df, aes(x = Year, y = Planted, color = county_name)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      labs(x = "Year", y = "Acres",
           title = "Soybean Acres Planted by County (2013–2025)") +
      theme_minimal(base_family = "serif")
    ggplotly(gg)
  })
  
  output$plot_harvested <- renderPlotly({
    req(input$county_sel)
    df <- soy_hist %>% filter(county_name %in% input$county_sel)
    gg <- ggplot(df, aes(x = Year, y = Harvested, color = county_name)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      labs(x = "Year", y = "Acres",
           title = "Soybean Acres Harvested by County (2013–2025)") +
      theme_minimal(base_family = "serif")
    ggplotly(gg)
  })
  
  output$plot_success <- renderPlotly({
    req(input$county_sel)
    df <- soy_hist %>% filter(county_name %in% input$county_sel)
    gg <- ggplot(df, aes(x = Year, y = SuccessRate, color = county_name)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      labs(x = "Year", y = "Percent",
           title = "Soybean Harvest Success Rate by County (2013–2025)") +
      theme_minimal(base_family = "serif")
    ggplotly(gg)
  })
  
  # Maps
  soy_data <- reactive({
    df <- get_soybean_acres_county(input$map_year)
    if (is.null(df)) return(NULL)
    
    wide <- df %>%
      group_by(Year, County, Var) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = Var, values_from = Value) %>%
      mutate(SuccessRate = ifelse(!is.na(Planted) & Planted > 0,
                                  (Harvested / Planted) * 100, NA))
    
    left_join(va_counties, wide, by = "County")
  })
  
  output$map_soy <- renderPlotly({
    df <- soy_data()
    if (is.null(df)) return(NULL)
    
    var <- input$map_var
    title <- switch(var,
                    "Planted"    = "Soybean Acres Planted",
                    "Harvested"  = "Soybean Acres Harvested",
                    "SuccessRate"= "Soybean Harvest Success Rate (%)")
    
    colorscale <- switch(var,
                         "Planted"    = "Greens",
                         "Harvested"  = "YlGn",
                         "SuccessRate"= "Blues")
    
    plot_ly(df, type = "choroplethmapbox",
            geojson = df, locations = ~County, z = df[[var]],
            colorscale = colorscale,
            text = ~paste0(County, "<br>", title, ": ",
                           ifelse(var == "SuccessRate",
                                  paste0(round(SuccessRate,1), "%"),
                                  scales::comma(df[[var]]))),
            hoverinfo = "text") %>%
      layout(mapbox = list(style = "carto-positron", zoom = 5.8,
                           center = list(lat = 37.5, lon = -78.5)))
  })
}

# --------------------------------------------------------------------
# Run App
# --------------------------------------------------------------------
shinyApp(ui, server)
