# ====================================================================
# functions.R
# ====================================================================
library(rnassqs)   
library(dplyr)     
library(lubridate) 

# Data Cleaning 
clean_title <- function(desc) {
  desc <- gsub("SOYBEANS - PROGRESS, MEASURED IN ", "", desc)
  desc <- gsub("PCT ", "", desc)
  tools::toTitleCase(tolower(desc))  
}
get_soy_categories <- function(year, state = "VIRGINIA") {
  data <- tryCatch(
    nassqs(list(
      commodity_desc    = "SOYBEANS",
      year              = year,
      state_name        = state,
      statisticcat_desc = "PROGRESS",
      unit_desc         = "PCT",
      agg_level_desc    = "STATE"
    )),
    error = function(e) return(NULL)
  )
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  unique(data$short_desc)
}

# ====================================================================
# Planting Progress Data (2020–2025)
# ====================================================================
soy_history <- read.csv("soybean_progress_fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.integer(Year),
    week  = as.Date(week),
    value = as.numeric(value)
  )

# ---- Combined (CSV for 2020–2024, API for 2025) ----
get_soy_progress_data <- function(year, category, state = "VIRGINIA") {
  tryCatch({
    if (year %in% 2020:2024) {
      soy_history %>%
        filter(Year == year, CategoryRaw == trimws(category))
    } else if (year == 2025) {
      nassqs(list(
        commodity_desc    = "SOYBEANS",
        year              = 2025,
        state_name        = state,
        statisticcat_desc = "PROGRESS",
        short_desc        = paste("SOYBEANS - PROGRESS, MEASURED IN", category),
        agg_level_desc    = "STATE"
      )) %>%
        mutate(
          week     = as.Date(week_ending, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
          value    = as.numeric(Value),
          Category = clean_title(category),
          Year     = 2025
        ) %>%
        filter(!is.na(week))
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# ---- 5-Year Average from CSV ----
get_soy_avg_data <- function(year, category) {
  tryCatch({
    years <- (year - 5):(year - 1)
    data <- soy_history %>%
      filter(Year %in% years, CategoryRaw == trimws(category))
    
    if (nrow(data) == 0) return(NULL)
    
    data %>%
      group_by(week) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Type = "5-Year Avg")
  }, error = function(e) NULL)
}
