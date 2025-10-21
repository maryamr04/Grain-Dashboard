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
      
/* Sidebar background */
.nav-stacked {
  background-color: #F3E5D0 !important;  /* beige like the body */
  border: none !important;
  box-shadow: none !important;
}

/* Remove white panel box around sidebar */
.navlist-panel {
  background-color: #F3E5D0 !important;
  border: none !important;
}

/* Make sidebar background beige */
.well {
  background-color: #F3E5D0 !important; 
  border: none !important;
  box-shadow: none !important;
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
        background-color:#6B4226; color:#3E2C23;
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
             h3("Soybean Planting Progress"),
             
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
             h3("Soybean Crop Conditions"),
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
             h3("County-Level Soybean Analysis"),
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
    
    # ---- Remote Sensing Trends Tab ----
    tabPanel("Remote Sensing Data",
             h3("Soybean Enhanced Difference Vegetation Index (EDVI)"),
             div(style = "
        background-color:#4B2E2B;
        color:#FDFBF7;
        font-family:'Times New Roman', serif;
        padding:15px;
        border-radius:8px;
        margin-bottom:20px;
        box-shadow:0 3px 8px rgba(0,0,0,0.2);
      ",
      h4("About EDVI"),
      p("EDVI (Enhanced Difference Vegetation Index) is similar to NDVI but 
         uses the blue band to better correct for atmospheric and soil background 
         effects. It is calculated as:"),
      p("EDVI = (NIR - RED) / sqrt(NIR + RED)"),
      p("Higher EDVI values indicate greener, healthier vegetation. 
         This plot shows weekly county-level averages for Virginia soybeans (2013–2025).")
             ),
      
      # EDVI controls + plot
      selectInput("edvi_year", "Select Year:", 2013:2025, selected = 2025),
      selectInput("edvi_county", "Select County:",
                  choices = NULL, multiple = TRUE),
      withSpinner(
        plotlyOutput("edvi_plot", height = "500px"),
        type = 4, color = "#FDFBF7", size = 0.7
      ),
      
      br(), hr(), br(),  # <-- nice break between EDVI and NDVI
      
      h3("Soybean Normalized Difference Vegetation Index (NDVI)"),
      div(style = "
        background-color:#4B2E2B;
        color:#FDFBF7;
        font-family:'Times New Roman', serif;
        padding:15px;
        border-radius:8px;
        margin-bottom:20px;
        box-shadow:0 3px 8px rgba(0,0,0,0.2);
      ",
      h4("About NDVI"),
      p("NDVI (Normalized Difference Vegetation Index) is one of the most 
         widely used vegetation indices. It is calculated as:"),
      p("NDVI = (NIR - RED) / (NIR + RED)"),
      p("Like EDVI, higher values indicate more vigorous vegetation, 
         but NDVI does not include the blue band correction.")
      ),
      
      # NDVI controls + plot
      selectInput("ndvi_year", "Select Year:", 2013:2025, selected = 2025),
      selectInput("ndvi_county", "Select County:",
                  choices = NULL, multiple = TRUE),
      withSpinner(
        plotlyOutput("ndvi_plot", height = "500px"),
        type = 4, color = "#FDFBF7", size = 0.7
      )
    ),
    
    # ---- Yield Forecast Tab ----
    tabPanel("Yield Forecasts",
             h3("Soybean Yield Forecasts"),
             
             div(style = "
      background-color:#4B2E2B;
      color:#FDFBF7;
      font-family:'Times New Roman', serif;
      padding:15px; border-radius:8px;
      margin-bottom:20px;
      box-shadow:0 3px 8px rgba(0,0,0,0.2);",
      h4("About this Data"),
      p("This section demonstrates three different approaches to forecasting Virginia soybean yields.
          Each model is built using historical USDA yield data (2014–2024) and additional weekly or
         satellite-based predictors.  The purpose is to show how different information sources
         contribute to prediction accuracy."),
      tags$ul(
        # Conditions-only model
        tags$li(
          strong("Conditions-only model:"),
          "This model uses only USDA weekly crop condition reports. Each week, soybeans are classified into categories
          such as Excellent, Good, Fair, Poor, and Very Poor. The share of soybeans in each category is used to estimate
          how much yield in that year will deviate from the long-term trend. Steps:"
        ),
        
        tags$li(
          strong("EDVI-only model:"),
          "This model relies entirely on satellite-derived vegetation health.
          The Enhanced Difference Vegetation Index (EDVI) is calculated from Landsat or MODIS imagery
          and summarizes canopy greenness and crop vigor. Unlike crop conditions, EDVI is objective and consistent. Steps:"
        ),
        
        tags$li(
          strong("Hybrid model (Conditions + EDVI):"),
          "This model combines both data sources — USDA weekly crop conditions and satellite-derived EDVI —
          into a single regression model. Steps:"
        )
      )
             ),
      
      p("Dashed red lines represent actual USDA-reported yields.
        Solid blue lines indicate the long-term trend.
        Green/orange dotted lines represent forecasts from each model.
        The comparison highlights whether adding EDVI improves accuracy over conditions alone."),
      
      # Model 1
      h4("Forecast Using Crop Conditions Only"),
      selectInput("year_forecast_conditions", "Select Year (Conditions):",
                  choices = sort(unique(soy_annual$Year)), selected = 2024),
      withSpinner(plotlyOutput("yield_forecast_conditions_plot", height = "350px"),
                  type = 4, color = "#FDFBF7", size = 0.7),
      textOutput("yield_forecast_conditions_summary"),
      
      br(), hr(), br(),
      
      # Model 2
      h4("Forecast Using EDVI Only"),
      selectInput("year_forecast_edvi", "Select Year (EDVI):",
                  choices = sort(unique(soy_annual$Year)), selected = 2024),
      withSpinner(plotlyOutput("yield_forecast_edvi_only_plot", height = "350px"),
                  type = 4, color = "#FDFBF7", size = 0.7),
      textOutput("yield_forecast_edvi_only_summary"),
      
      br(), hr(), br(),
      
      # Model 3
      h4("Forecast Using Conditions + EDVI"),
      selectInput("year_forecast_cond_edvi", "Select Year (Conditions + EDVI):",
                  choices = sort(unique(soy_annual$Year)), selected = 2024),
      withSpinner(plotlyOutput("yield_forecast_cond_edvi_plot", height = "350px"),
                  type = 4, color = "#FDFBF7", size = 0.7),
      textOutput("yield_forecast_cond_edvi_summary"),
      
      br(), hr(), br(),
      
      # Model 4
      h4("Forecast Using ARIMAX (Time-Series + Predictors)"),
      div(style = "
     background-color:#4B2E2B;   /* dark brown */
     color:#FDFBF7;              /* white text */
     font-family:'Times New Roman', serif;
     padding:15px;
     border-radius:8px;
     margin-bottom:20px;
     box-shadow:0 3px 8px rgba(0,0,0,0.2);",
     p("This model goes beyond simple regression by using an ARIMAX approach.
       ARIMAX combines time-series dynamics (Auto-Regressive Integrated Moving Average)
       with external predictors such as EDVI and USDA crop condition categories.
       By incorporating both the historical yield trajectory and yearly predictors,
       it can capture yield momentum and shocks while adjusting for explanatory variables.
       This often results in more accurate forecasts compared to models that rely
       only on trend, conditions, or EDVI.")
      ),
     withSpinner(plotlyOutput("yield_forecast_arimax_plot", height = "350px"),
                 type = 4, color = "#FDFBF7", size = 0.7),
     textOutput("yield_forecast_arimax_summary"),
     
     br(), hr(), br(),
     
     h4("Model Comparison: All Forecasting Approaches (by Available Years)"),
     
     div(style = "
  background-color:#4B2E2B;
  color:#FDFBF7;
  font-family:'Times New Roman', serif;
  padding:15px;
  border-radius:8px;
  margin-bottom:20px;
  box-shadow:0 3px 8px rgba(0,0,0,0.2);
",
p("This visualization compares all four models — Conditions-only, EDVI-only, 
   Conditions + EDVI, and ARIMAX — along with the actual and trend yields. 
   Each line only covers the years where data exist for that model, 
   so the time spans differ slightly across models."),
p("• Black line = Actual yield"),
p("• Blue dashed line = Trend yield"),
p("• Green line = Conditions-only forecast"),
p("• Teal line = EDVI-only forecast"),
p("• Orange line = Hybrid forecast (Conditions + EDVI)"),
p("• Red line = ARIMAX forecast (time-series)")
     ),
     
     withSpinner(
       plotlyOutput("yield_forecast_comparison_plot", height = "400px"),
       type = 4, color = "#FDFBF7", size = 0.7
     ),
     textOutput("yield_forecast_comparison_summary")
     
     
    ),
    
    
    
    tabPanel("Feedback / FAQ",
             h3("💬 Feedback Form"),
             textInput("user_name", "Your Name (optional):"),
             textAreaInput("user_feedback", "Your Feedback:", "", width = "100%", height = "120px"),
             actionButton("submit_feedback", "Submit"),
             br(), br(),
             textOutput("feedback_message"),
             hr(),
             
             h3("📖 FAQ"),
             selectInput("faq_question", "Choose a question:", 
                         choices = names(faq_answers), selected = names(faq_answers)[1]),
             textOutput("faq_answer")
    ),
    
    
    # ---- Placeholder Tabs ----
    tabPanel("About this data",           h3("ℹ️ About (Placeholder)"))
  )
)

