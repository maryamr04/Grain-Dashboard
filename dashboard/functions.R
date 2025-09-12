# ====================================================================
# functions.R
# ====================================================================
library(rnassqs)
library(dplyr)
library(lubridate)
library(tidyr)

# ------------------------- Helpers -----------------------------------
clean_title <- function(desc) {
  desc <- gsub("SOYBEANS - PROGRESS, MEASURED IN ", "", desc)
  desc <- gsub("PCT ", "", desc)
  tools::toTitleCase(tolower(desc))
}

clean_value <- function(x) as.numeric(gsub(",", "", x))

qs_common <- function(state = "VIRGINIA") {
  list(
    commodity_desc = "SOYBEANS",
    state_name     = state,
    agg_level_desc = "STATE",
    source_desc    = "SURVEY"
  )
}

# -------------------- Load historical CSVs ---------------------------
# Actual progress (2020–2024)
soy_progress <- read.csv("soybean_progress_fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.character(Year),
    week  = as.Date(week),
    value = as.numeric(value)
  )

# 5-year averages (always from CSV)
soy_avg <- read.csv("soybean_progress_avg.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.character(Year), 
    week  = as.Date(week),
    value = as.numeric(value)
  )

# Crop conditions (2020–2025)
soy_conditions <- read.csv("soybean_conditions_fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.character(Year),
    week  = as.Date(week),
    value = as.numeric(value)
  )

# ---------------- Planting Progress (Actual) ---------------------
get_soy_progress_data <- function(year, category, state = "VIRGINIA") {
  if (year %in% 2020:2024) {
    soy_progress %>%
      filter(Year == as.character(year), CategoryRaw == category) %>%
      mutate(Type = "Actual")
    
  } else if (year == 2025) {
    tryCatch({
      rnassqs::nassqs(c(
        list(
          commodity_desc    = "SOYBEANS",
          state_name        = state,
          agg_level_desc    = "STATE",
          source_desc       = "SURVEY",
          year              = year,
          statisticcat_desc = "PROGRESS",
          unit_desc         = "PCT",
          short_desc        = paste("SOYBEANS - PROGRESS, MEASURED IN", category)
        )
      )) %>%
        filter(grepl("^[0-9,]+$", Value)) %>%
        transmute(
          week        = as.Date(week_ending),
          value       = clean_value(Value),
          Type        = "Actual",
          Year        = as.character(year),
          CategoryRaw = category
        ) %>%
        filter(!is.na(week))
    }, error = function(e) NULL)
    
  } else {
    NULL
  }
}

# ---------------- Planting Progress (5-Year Avg) ---------------------
get_soy_avg_data <- function(year, category) {
  soy_avg %>%
    filter(CategoryRaw == category) %>%
    mutate(
      # shift the week date into the requested year
      week = as.Date(paste0(year, format(week, "-%m-%d"))),
      Year = as.character(year),
      Type = "5-Year Avg"
    )
}

# ---------------------- Crop Conditions ------------------------------
get_soybean_conditions <- function(year) {
  df <- soy_conditions %>%
    filter(Year == as.character(year))
  
  if (nrow(df) == 0) return(NULL)
  
  levels5 <- c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")
  
  df <- df %>%
    group_by(week, condition) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    tidyr::complete(week, condition = levels5, fill = list(value = 0)) %>%
    mutate(condition = factor(condition, levels = levels5),
           Year = as.character(year))
  
  df
}
