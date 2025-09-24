# ====================================================================
# ui.R
# ====================================================================
library(shiny)
library(plotly)
library(shinycssloaders)
library(tidyr)

years <- 2014:2025
categories <- c("PCT PLANTED", "PCT EMERGED", "PCT BLOOMING",
                "PCT SETTING PODS", "PCT DROPPING LEAVES", "PCT HARVESTED")

# ====================================================================
# Aesthetics / CSS Styling
# ====================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
      body { background-color: #F3E5D0 !important; }

      /* ---- Title Box ---- */
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

      /* ---- Sidebar Navigation Buttons ---- */
      .nav-pills > li > a {
        background-color:#4B2E2B; color:#FDFBF7;
        font-family:'Times New Roman', serif;
        font-weight:bold; border-radius:6px;
        margin-bottom:8px; padding:12px;
        transition:all 0.2s ease-in-out;
        border:1px solid #3E2C23;
      }
      .nav-pills > li.active > a {
        background-color:#3E2C23 !important; color:#FDFBF7 !important;
      }
      .nav-pills > li > a:hover {
        background-color:#6B4226; color:white;
      }
      
      /* ---- Tab Content Panels ---- */
      .tab-content > .tab-pane {
        background-color:#4B2E2B; color:#FDFBF7;
        border:1px solid #3E2C23; border-radius:6px;
        padding:20px; margin-top:15px;
        box-shadow:0 3px 8px rgba(0,0,0,0.2);
      }
      .tab-content h2, .tab-content h3, .tab-content h4 {
        color:#FDFBF7; font-family:'Times New Roman', serif; font-weight:700;
      }
      .tab-content p {
        font-family:'Times New Roman', serif;
        font-size:16px; color:#FDFBF7;
      }
      
  "))),
  
  # ====================================================================
  #  Title Section
  # ====================================================================
  div(class = "title-box",
      h1("🌱 Soybean Interactive Dashboard"),
      h4("A tool brought to you by the Virginia Tech Kohl Centre")
  ),
  
  # ====================================================================
  #  Sidebar Navigation
  # ====================================================================
  navlistPanel(
    widths = c(2, 10),
    
    # ---- Objective Tab ----
    tabPanel("Objective",
             h2("🌱 Welcome"),
             p("Explore soybean planting progress (2013–2025) across stages, 
                compared to 5-year averages. Data is from the USDA NASS API.")
    ),
    
    # ---- Planting Progress Tab ----
    tabPanel("Planting Progress",
             h3("🌱 Planting Progress"),
             
             div(style = "
        background-color:#4B2E2B;
        color:#FDFBF7;
        font-family:'Times New Roman', serif;
        padding:15px;
        border-radius:8px;
        margin-bottom:20px;
        box-shadow:0 3px 8px rgba(0,0,0,0.2);
      ",
      h4("About this Data"),
      p("This dashboard uses soybean planting progress and crop development data 
         from USDA NASS. Percentages represent the share of soybean acres at each growth stage in Virginia.")
             ),
      
      # Year selector
      selectInput("year_sel", "Select Year:",
                  choices = 2014:2025, selected = 2025),
      
      # All category plots stacked vertically
      # All category plots stacked vertically
      lapply(progress_categories, function(cat) {
        safe_id <- paste0("soy_progress_", gsub("[^A-Za-z]", "_", cat))
        tagList(
          h4(cat),   # <-- FIXED: use raw category name here
          withSpinner(
            plotlyOutput(safe_id, height = "300px"),
            type = 4, color = "#FDFBF7", size = 0.7
          ),
          br()
        )
      })
    ),
    
    
    # ---- Crop Conditions Tab ----
    tabPanel("Crop Conditions",
             h3("🌾 Soybean Crop Conditions"),
             div(style = "
         background-color:#4B2E2B;
         color:#FDFBF7;
         font-family:'Times New Roman', serif;
         padding:15px;
         border-radius:8px;
         margin-bottom:20px;
         box-shadow:0 3px 8px rgba(0,0,0,0.2);
       ",
       h4("About this Data"),
       p("Soybean crop conditions (2013–2025) are pulled from the USDA NASS API. 
          Categories include Very Poor, Poor, Fair, Good, and Excellent. 
          Percentages represent the share of soybean acres in each condition.")
             ),
       
       # User Control
       selectInput("year_sel_conditions", "Select Year:", choices = years, selected = 2025),
       
       # Conditions Plot
       withSpinner(
         plotlyOutput("plot_conditions", height = "400px"),
         type = 4, color = "#FDFBF7", size = 0.7
       )
    ),
    
    # ---- County Analysis Tab ----
    tabPanel("County Analysis",
             h3("🏞️ County-Level Soybean Analysis"),
             div(style = "
           background-color:#4B2E2B;
           color:#FDFBF7;
           font-family:'Times New Roman', serif;
           padding:15px;
           border-radius:8px;
           margin-bottom:20px;
           box-shadow:0 3px 8px rgba(0,0,0,0.2);
         ",
         h4("About this Data"),
         p("This section compares soybean acres planted, harvested, 
            and harvest success rates (2014–2024 CSV, 2025 API in future). 
            Select a state and one or more counties to visualize trends over time.")
             ),
         
         # State + County selectors
         selectInput("state_sel", "Select State:", choices = unique(soy_county$State)),
         selectInput("county_sel", "Select County:", 
                     choices = unique(soy_county$County),
                     multiple = TRUE, 
                     selected = unique(soy_county$County)[1]),
         
         # Plots
         withSpinner(plotlyOutput("county_planted_plot", height = "300px"),
                     type = 4, color = "#FDFBF7", size = 0.7),
         br(),
         withSpinner(plotlyOutput("county_harvested_plot", height = "300px"),
                     type = 4, color = "#FDFBF7", size = 0.7),
         br(),
         withSpinner(plotlyOutput("county_success_plot", height = "300px"),
                     type = 4, color = "#FDFBF7", size = 0.7),
         br(),
         
         # Map
         selectInput("map_year_sel", "Select Year (Map):",
                     choices = 2013:2024,
                     selected = 2024),
         
         withSpinner(
           leafletOutput("county_map", height = "500px"),   # <-- FIXED
           type = 4, color = "#FDFBF7", size = 0.7 )
         
    ),
    
    
    # ---- Placeholder Tabs ----
    tabPanel("Yield Trends",    h3("📈 Yield Trends (Placeholder)")),
    tabPanel("Remote Sensing",  h3("🛰️ Remote Sensing (Placeholder)")),
    tabPanel("About",           h3("ℹ️ About (Placeholder)"))
  )
)
