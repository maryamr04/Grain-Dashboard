# functions.R
library(rnassqs)
library(dplyr)
library(lubridate)

# Map friendly category names to USDA API short_desc values
soy_category_map <- list(
  "PLANTED"         = "SOYBEANS - PROGRESS, MEASURED IN PCT PLANTED",
  "EMERGED"         = "SOYBEANS - PROGRESS, MEASURED IN PCT EMERGED",
  "BLOOMING"        = "SOYBEANS - PROGRESS, MEASURED IN PCT BLOOMING",
  "SETTING PODS"    = "SOYBEANS - PROGRESS, MEASURED IN PCT SETTING PODS",
  "DROPPING LEAVES" = "SOYBEANS - PROGRESS, MEASURED IN PCT DROPPING LEAVES",
  "HARVESTED"       = "SOYBEANS - PROGRESS, MEASURED IN PCT HARVESTED"
)

# Helper: clean stage titles for plotting
clean_title <- function(cat) {
  tools::toTitleCase(tolower(cat))
}

# Get soybean progress for a year + category
get_soy_progress_data <- function(year, category, state = "VIRGINIA") {
  desc <- soy_category_map[[category]]
  if (is.null(desc)) return(NULL)   # if mapping not found
  
  data <- tryCatch(
    nassqs(list(
      commodity_desc    = "SOYBEANS",
      year              = year,
      state_name        = state,
      statisticcat_desc = "PROGRESS",
      unit_desc         = "PCT",
      short_desc        = desc,
      agg_level_desc    = "STATE"
    )),
    error = function(e) return(NULL)
  )
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  data <- data %>%
    mutate(week = as.Date(week_ending)) %>%   # ✅ week_ending already a date
    select(week, value = Value)
  
  return(data)
}

# Get 5-year avg soybean progress
get_soy_avg_data <- function(year, category, state = "VIRGINIA") {
  years <- (year - 5):(year - 1)
  data <- lapply(years, function(y) get_soy_progress_data(y, category, state))
  data <- bind_rows(data)
  if (nrow(data) == 0) return(NULL)
  
  avg <- data %>%
    group_by(week) %>%
    summarise(value = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")
  
  return(avg)
}
