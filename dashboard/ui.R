# ui.R
# Purpose: Defines the visual layout and inputs/outputs for the app.
library(shiny)
library(bslib)

# ---- Theme ----
grain_theme <- bs_theme(
  version      = 5,
  base_font    = "Times New Roman, serif",
  heading_font = "Times New Roman, serif",
  bg           = "#F5E6D3",  # light tan background
  fg           = "#1F2A32",
  primary      = "#2E6B3B",  # dark green
  secondary    = "#4B2E2B"   # dark brown
)

ui <- page(
  theme = grain_theme,
  
  # ---- Title banner ----
  div(
    style = "
      background:#4B2E2B;
      color:#FDFBF7;
      padding:30px 20px;
      text-align:center;
      margin-bottom:20px;
    ",
    tags$h1("🌾 Grain Interactive Dashboard",
            style="margin:0; font-size:42px; font-family:'Times New Roman'; font-weight:700;"),
    tags$h4("A tool brought to you by the Virginia Tech Kohl Centre",
            style="margin-top:8px; font-size:20px; font-family:'Times New Roman';
                   font-style:italic; color:#EADBC8; font-weight:400;")
  ),
  
  # ---- Collapsible Sidebar Navigation ----
  navset_sidebar(
    id = "main_tabs",
    sidebar = sidebar(title = "Navigation", open = "open", width = 260),
    
    # ---------- HOME TAB ----------
    nav_panel(
      "Home",
      layout_columns(
        col_widths = c(6, 6),
        div(
          style = "background-color:#FFFFFF;
                   border-radius:12px; padding:20px; margin:20px;
                   box-shadow:0 2px 6px rgba(0,0,0,0.15);",
          h3("Project Overview"),
          p("This dashboard provides an interactive interface to explore grain-related data — 
             covering planting progress, crop conditions, yields, and remote sensing."),
          tags$hr(),
          tags$strong("Made by: Virginia Tech Kohl Centre")
        ),
        div(
          style = "background-color:#FFFFFF;
                   border-radius:12px; padding:20px; margin:20px;
                   box-shadow:0 2px 6px rgba(0,0,0,0.15);",
          h3("Data Notes"),
          tags$ul(
            tags$li("Updated regularly from USDA NASS API"),
            tags$li("Covers Virginia, Maryland, and North Carolina"),
            tags$li("Historical data from 2000–present"),
            tags$li("Future expansion: NDVI & temperature from remote sensing")
          )
        )
      )
    ),
    
    # ---------- OTHER PLACEHOLDERS ----------
    nav_panel("Planting Progress", h3("Planting Progress (Placeholder)")),
    nav_panel("Crop Conditions", h3("Crop Conditions (Placeholder)")),
    nav_panel("Yield Trends", h3("Yield Trends (Placeholder)")),
    nav_panel("Remote Sensing", h3("Remote Sensing (Placeholder)")),
    nav_panel("About", h3("About (Placeholder)"))
  )
)
