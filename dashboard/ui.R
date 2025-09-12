# ====================================================================
# ui.R
# ====================================================================
library(shiny)
library(plotly)
library(shinycssloaders)
library(tidyr)


years <- 2020:2025
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

      /* ---- Sidebar Container ---- */
      .nav-pills {
        background-color: transparent !important;
        border: none !important;
        padding: 10px;
      }

      /* Make tab titles white */
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

      /* ---- Sidebar List Items ---- */
      .nav-pills > li {
        margin-bottom: 8px;
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
             p("Explore soybean planting progress (2020–2025) across stages, 
         compared to 5-year averages. Data is from the USDA NASS API.")
    ),
    
    # ---- Planting Progress Tab ----
    tabPanel("Planting Progress",
             h3("🌱 Planting Progress"),
             
             # About this Data Info Box
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
          from the USDA NASS API. All percentages represent the share of soybean acres 
          at each growth stage in Virginia.")
             ),
       
       # Yearly Tabs (2020–2025)
       do.call(tabsetPanel, c(
         id = "year_tabs",
         lapply(years, function(yr) {
           tabPanel(
             title = paste(yr),
             lapply(categories, function(cat) {
               safe_id <- paste0("soy_progress_", yr, "_", gsub("[^A-Za-z]", "_", cat))
               tagList(
                 h4(paste(clean_title(cat), "—", yr)),
                 withSpinner(
                   plotlyOutput(safe_id, height = "300px"),
                   type = 4, color = "#FDFBF7", size = 0.7
                 ),
                 br()
               )
             })
           )
         })
       ))
    ),
    
    # ---- rop Conditions Tab ----
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
       p("Soybean crop conditions (2020–2025) are pulled directly from the USDA NASS API. 
          Categories include Very Poor, Poor, Fair, Good, and Excellent. 
          Percentages represent the share of soybean acres in each condition.")
             ),
       
       # Year Tabs (2020–2025)
       do.call(tabsetPanel, c(
         id = "condition_year_tabs",
         lapply(years, function(yr) {
           tabPanel(
             title = paste(yr),
             withSpinner(
               plotlyOutput(paste0("soy_conditions_", yr), height = "400px"),
               type = 4, color = "#FDFBF7", size = 0.7
             ),
             br()
           )
         })
       ))
    ),
    
    # ---- Placeholder Tabs ----
    tabPanel("Yield Trends",    h3("📈 Yield Trends (Placeholder)")),
    tabPanel("Remote Sensing",  h3("🛰️ Remote Sensing (Placeholder)")),
    tabPanel("About",           h3("ℹ️ About (Placeholder)"))
  )
)
