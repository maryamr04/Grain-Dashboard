#Planting Progress

# ---- Libraries ----
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(lubridate)
library(shinycssloaders)

# ---- Auth ----
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# ---- Farming Order Categories ----
progress_categories <- c(
  "PCT SEEDBED PREPARED", "PCT PLANTED", "PCT EMERGED",
  "PCT BLOOMING", "PCT SETTING PODS", "PCT FULLY PODDED",
  "PCT COLORING", "PCT MATURE", "PCT DROPPING LEAVES",
  "PCT HARVESTED"
)

clean_title <- function(cat) {
  title <- gsub("^PCT ", "", cat)
  title <- tolower(title)
  title <- gsub("_", " ", title)
  paste0(toupper(substr(title, 1, 1)), substr(title, 2, nchar(title)))
}

# ---- Data Functions ----
get_soy_progress_data <- function(year, category, state = "VIRGINIA") {
  tryCatch({
    nassqs(list(
      commodity_desc = "SOYBEANS",
      year = year,
      state_name = state,
      statisticcat_desc = "PROGRESS",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value),
        Category = clean_title(category),
        Year = year
      ) %>%
      filter(!is.na(week))
  }, error = function(e) NULL)
}

get_soy_avg_data <- function(year, category, state = "VIRGINIA") {
  tryCatch({
    nassqs(list(
      commodity_desc = "SOYBEANS",
      year = year,
      state_name = state,
      statisticcat_desc = "PROGRESS, 5 YEAR AVG",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value),
        Category = clean_title(category),
        Year = year
      ) %>%
      filter(!is.na(week))
  }, error = function(e) NULL)
}

# ---- Server ----
server <- function(input, output, session) {
  
  output$progress_plot <- renderPlotly({
    req(input$year, input$category)
    
    actual <- get_soy_progress_data(input$year, input$category)
    avg <- get_soy_avg_data(input$year, input$category)
    
    # Handle no data
    if ((is.null(actual) || nrow(actual) == 0) && (is.null(avg) || nrow(avg) == 0)) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(
                 title = list(text = paste("No data available for", clean_title(input$category), "-", input$year)),
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE)
               ))
    }
    
    combined <- bind_rows(
      if (!is.null(actual)) mutate(actual, Type = "Actual") else NULL,
      if (!is.null(avg)) mutate(avg, Type = "5-Year Avg") else NULL
    )
    
    ggplotly(
      ggplot(combined, aes(x = week, y = value, color = Type, linetype = Type)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Actual" = "#2E7D32", "5-Year Avg" = "#8D6E63")) +
        labs(
          title = paste(clean_title(input$category), "Progress —", input$year),
          x = "Week Ending", y = "%"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2E7D32"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "#F3E5D0", color = NA),
          plot.background = element_rect(fill = "#F3E5D0", color = NA)
        ),
      tooltip = c("x", "y", "color")
    )
  })
}

# ---- UI ----
ui <- navbarPage(
  title = "🌱 Soybean Planting Progress Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  id = "year",
  
  # One tab per year
  !!!lapply(2020:2025, function(yr) {
    tabPanel(
      title = as.character(yr),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "category", "Select Progress Stage:",
            choices = progress_categories,
            selected = "PCT PLANTED"
          ),
          width = 3
        ),
        mainPanel(
          withSpinner(plotlyOutput("progress_plot", height = "600px"),
                      type = 4, color = "#2E7D32", size = 0.7),
          width = 9
        )
      )
    )
  })
)

# ---- Run ----
shinyApp(ui, server)
