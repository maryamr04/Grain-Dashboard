#functions.r
library(rnassqs)
library(dplyr)
library(lubridate)

# Helper: clean USDA short_desc into simpler labels for plotting
clean_title <- function(desc) {
  desc <- gsub("SOYBEANS - PROGRESS, MEASURED IN ", "", desc)
  desc <- gsub("PCT ", "", desc)
  tools::toTitleCase(tolower(desc))
}

# Get all available soybean categories for a given year
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

# Get soybean progress for a year + USDA short_desc
# ---- Data Functions ----
get_soy_progress_data <- function(year, category, state = "VIRGINIA") {
  tryCatch({
    nassqs(list(
      commodity_desc    = "SOYBEANS",
      year              = year,
      state_name        = state,
      statisticcat_desc = "PROGRESS",
      short_desc        = paste("SOYBEANS - PROGRESS, MEASURED IN", category),
      agg_level_desc    = "STATE"
    )) %>%
      mutate(
        week = as.Date(week_ending, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
        value = as.numeric(Value),
        Category = clean_title(category),
        Year = year
      ) %>%
      filter(!is.na(week))
  }, error = function(e) NULL)
}

get_soy_avg_data <- function(year, category, state = "VIRGINIA") {
  tryCatch({
    years <- (year - 5):(year - 1)
    data <- lapply(years, function(y) {
      nassqs(list(
        commodity_desc    = "SOYBEANS",
        year              = y,
        state_name        = state,
        statisticcat_desc = "PROGRESS",
        short_desc        = paste("SOYBEANS - PROGRESS, MEASURED IN", category),
        agg_level_desc    = "STATE"
      ))
    })
    data <- bind_rows(data)
    if (nrow(data) == 0) return(NULL)
    
    data %>%
      mutate(
        week = as.Date(week_ending, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
        value = as.numeric(Value),
        Category = clean_title(category),
        Year = year
      ) %>%
      group_by(week) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  }, error = function(e) NULL)
}

# Get 5-year avg soybean progress (using USDA short_desc)
get_soy_avg_data <- function(year, short_desc, state = "VIRGINIA") {
  years <- (year - 5):(year - 1)
  data <- lapply(years, function(y) get_soy_progress_data(y, short_desc, state))
  data <- bind_rows(data)
  if (nrow(data) == 0) return(NULL)
  
  avg <- data %>%
    group_by(week) %>%
    summarise(value = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")
  
  return(avg)
}
