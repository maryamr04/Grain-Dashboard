# ====================================================================
# functions.R
# ====================================================================

library(rnassqs)
library(dplyr)
library(lubridate)
options(tigris_use_cache = TRUE)

# ------------------------- Helpers -----------------------------------

# Clean titles (remove long USDA prefixes, fix casing)
clean_title <- function(desc) {
  desc <- gsub("SOYBEANS - PROGRESS, MEASURED IN ", "", desc)
  desc <- gsub("PCT ", "", desc)
  tools::toTitleCase(tolower(desc))
}

# Convert USDA "1,234" style numbers into numeric
clean_value <- function(x) as.numeric(gsub(",", "", x))

# Shared query template (for API pulls)
qs_common <- function(state = "VIRGINIA") {
  list(
    commodity_desc = "SOYBEANS",
    state_name     = state,
    agg_level_desc = "STATE",
    source_desc    = "SURVEY"
  )
}

# ====================================================================
#  Planting Progress Module (CSV 2014–2024, API 2025)
# ====================================================================

# Load cleaned + combined CSV (2014–2024 Actual + 5-Year Avg)
soy_progress <- read.csv("soybeans_progress_with_5yravg_final.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.character(Year),
    week  = as.Date(week),
    value = as.numeric(value),
    five_year_avg_value = as.numeric(five_year_avg_value)
  )

# All planting progress categories tracked by USDA
progress_categories <- c(
  "PCT PLANTED",
  "PCT EMERGED",
  "PCT BLOOMING",
  "PCT SETTING PODS",
  "PCT DROPPING LEAVES",
  "PCT HARVESTED"
)

# ---------------------- Planting Progress ----------------------------

# Get Actual + 5-Year Avg from CSV (2014–2024)
get_soy_progress_data <- function(year, category) {
  if (year %in% 2014:2024) {
    soy_progress %>%
      filter(Year == as.character(year), CategoryRaw == category, Type == "Actual") %>%
      mutate(Type = "Actual")
  } else if (year == 2025) {
    # API pull for 2025
    tryCatch({
      rnassqs::nassqs(list(
        commodity_desc    = "SOYBEANS",
        state_name        = "VIRGINIA",
        agg_level_desc    = "STATE",
        source_desc       = "SURVEY",
        year              = year,
        statisticcat_desc = "PROGRESS",
        unit_desc         = category
      )) %>%
        transmute(
          week        = as.Date(week_ending),
          value       = clean_value(Value),
          Type        = "Actual",
          Year        = as.character(year),
          CategoryRaw = category
        ) %>%
        filter(!is.na(week))
    }, error = function(e) {
      message("Error fetching 2025 planting progress: ", e$message)
      NULL
    })
  }
}

# Get 5-Year Average (from combined CSV only)
get_soy_avg_data <- function(year, category) {
  soy_progress %>%
    filter(CategoryRaw == category, Type == "5-Year Avg") %>%
    mutate(Year = as.character(year))
}


# ====================================================================
# Soybean Conditions Module (CSV 2013–2024, API 2025)
# ====================================================================

# Load cleaned CSV
soy_conditions <- read.csv("soybean_conditions_fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year  = as.character(Year),
    week  = as.Date(week),
    value = as.numeric(value),
    condition = toupper(condition)
  )

# Standardize levels
condition_levels <- c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")

# Function: returns conditions for a given year
get_soybean_conditions <- function(year, state = "VIRGINIA") {
  if (year %in% 2013:2024) {
    # From CSV
    df <- soy_conditions %>%
      filter(Year == as.character(year)) %>%
      group_by(week, condition) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      tidyr::complete(week, condition = condition_levels, fill = list(value = 0)) %>%
      mutate(condition = factor(condition, levels = condition_levels),
             Year = as.character(year))
    return(df)
    
  } else if (year == 2025) {
    # From API
    df <- tryCatch({
      rnassqs::nassqs(list(
        commodity_desc    = "SOYBEANS",
        state_name        = state,
        agg_level_desc    = "STATE",
        source_desc       = "SURVEY",
        year              = year,
        statisticcat_desc = "CONDITION"   # ✅ condition filter
        # no need for unit_desc = "PCT"
      ))
    }, error = function(e) {
      message("Error fetching 2025 conditions: ", e$message)
      return(NULL)
    })
    
    if (!is.null(df) && nrow(df) > 0) {
      df <- df %>%
        filter(grepl("^PCT ", unit_desc)) %>%   # ✅ only keep % condition rows
        transmute(
          week      = as.Date(week_ending),
          value     = as.numeric(gsub(",", "", Value)),
          condition = gsub("PCT ", "", toupper(unit_desc)),  # ✅ parse from unit_desc
          Year      = as.character(year)
        ) %>%
        filter(!is.na(week)) %>%
        group_by(week, condition) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        tidyr::complete(week, condition = condition_levels, fill = list(value = 0)) %>%
        mutate(condition = factor(condition, levels = condition_levels),
               Year = as.character(year))
      return(df)
    } else {
      return(NULL)
    }
    
  } else {
    return(NULL)
  }
}


# ====================================================================
#  County Analysis Module (2014–2024 CSV, 2025 API later)
# ====================================================================

soy_county <- read.csv("soybean_county_fixed.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Year        = as.character(Year),
    Planted     = as.numeric(Planted),
    Harvested   = as.numeric(Harvested),
    SuccessRate = as.numeric(SuccessRate),
    GEOID       = stringr::str_pad(GEOID, 5, pad = "0"),
    County      = tolower(trimws(County)),
    State       = case_when(
      State == "VIRGINIA" ~ "VA",
      State == "NORTH CAROLINA" ~ "NC",
      State == "MARYLAND" ~ "MD",
      TRUE ~ State
    )
  ) %>%
  group_by(State, County, Year) %>%
  summarise(
    Planted     = sum(Planted, na.rm = TRUE),
    Harvested   = sum(Harvested, na.rm = TRUE),
    SuccessRate = mean(SuccessRate, na.rm = TRUE),
    .groups = "drop"
  )


get_county_planted <- function(year) {
  soy_county %>%
    filter(Year == as.character(year)) %>%
    select(State, County, GEOID, Year, Planted)
}

get_county_harvested <- function(year) {
  soy_county %>%
    filter(Year == as.character(year)) %>%
    select(State, County, GEOID, Year, Harvested)
}

get_county_success <- function(year) {
  soy_county %>%
    filter(Year == as.character(year)) %>%
    select(State, County, GEOID, Year, SuccessRate)
}
all_counties <- readRDS("all_counties.rds")


# ====================================================================
#  Remote Sensing Module (EDVI + NDVI 2013–2025 CSV)
# ====================================================================

# Load cleaned EDVI/NDVI dataset
soy_edvi <- read.csv("Soybeans_WeeklyBands_2013_2025_clean_EDVI.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(
    date        = as.Date(date),
    week        = as.integer(week),
    year        = as.integer(year),
    mean_EDVI   = as.numeric(mean_EDVI),
    mean_NDVI   = as.numeric(NDVI),  # <-- NEW COLUMN
    county_name = as.character(county_name)
  )

# Function to filter EDVI data for given year & counties
get_soy_edvi <- function(year, counties = NULL) {
  df <- soy_edvi %>% filter(year == !!year)
  if (!is.null(counties)) {
    df <- df %>% filter(county_name %in% counties)
  }
  return(df %>% select(year, week, county_name, mean_EDVI))
}

# Function to filter NDVI data for given year & counties
get_soy_ndvi <- function(year, counties = NULL) {
  df <- soy_edvi %>% filter(year == !!year)
  if (!is.null(counties)) {
    df <- df %>% filter(county_name %in% counties)
  }
  return(df %>% select(year, week, county_name, mean_NDVI))
}
