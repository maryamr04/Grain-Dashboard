# ====================================================================
# server.R
# ====================================================================

source("functions.R")
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

years <- 2020:2025
categories <- c("PCT PLANTED", "PCT EMERGED", "PCT BLOOMING",
                "PCT SETTING PODS", "PCT DROPPING LEAVES", "PCT HARVESTED")

server <- function(input, output, session) {
  
  # ---------------------- Planting Progress --------------------------
  for (yr in years) {
    for (cat in categories) {
      safe_id <- paste0("soy_progress_", yr, "_", gsub("[^A-Za-z]", "_", cat))
      
      local({
        year_inner     <- yr
        category_inner <- cat
        output_id      <- safe_id
        
        output[[output_id]] <- renderPlotly({
          actual <- get_soy_progress_data(year_inner, category_inner)
          avg <- get_soy_avg_data(year_inner, category_inner)
          

          
          
          if ((is.null(actual) || nrow(actual) == 0) &&
              (is.null(avg)    || nrow(avg)    == 0)) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No data for",
                                                      clean_title(category_inner),
                                                      "-", year_inner))))
          }
          
          combined <- bind_rows(
            if (!is.null(actual) && nrow(actual) > 0) actual else NULL,
            if (!is.null(avg)    && nrow(avg)    > 0) avg    else NULL
          )
          
          gg <- ggplot(combined, aes(x = week, y = value, color = Type)) +
            geom_line(linewidth = 1.2) +
            geom_point(size = 2.5) +
            scale_color_manual(
              values = c("Actual" = "#2E7D32", "5-Year Avg" = "#8D6E63"),
              name = "Legend"
            ) +
            scale_x_date(date_labels = "%b", date_breaks = "1 month",
                         expand = c(0.01, 0.01)) +
            labs(
              title = paste(clean_title(category_inner), "—", year_inner),
              x = "Month",
              y = "Value"
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_rect(fill = "#F3E5D0", color = NA),
              plot.background  = element_rect(fill = "#F3E5D0", color = NA)
            )
          
          ggplotly(gg, tooltip = c("x", "y", "color"))
        })
      })
    }
  }
  
  # ------------------------ Crop Conditions --------------------------
  for (yr in years) {
    local({
      year_inner <- yr
      output_id  <- paste0("soy_conditions_", yr)
      
      output[[output_id]] <- renderPlotly({
        df <- get_soybean_conditions(year_inner)
        
        if (is.null(df) || nrow(df) == 0) {
          return(plotly_empty(type = "scatter", mode = "lines") %>%
                   layout(title = list(text = paste("No data for Soybean Conditions —",
                                                    year_inner))))
        }
        
        plot_ly(
          data   = df %>% arrange(week, condition),
          x      = ~week,
          y      = ~value,
          color  = ~condition,
          colors = c(
            "VERY POOR" = "#6D4C41",
            "POOR"      = "#D95F02",
            "FAIR"      = "#FFD54F",
            "GOOD"      = "#66BB6A",
            "EXCELLENT" = "#1B5E20"
          ),
          type       = "scatter",
          mode       = "none",
          stackgroup = "one",
          fill       = "tonexty",
          text = ~paste0(
            "<b>Week:</b> ", format(week, "%b %d, %Y"),
            "<br><b>Condition:</b> ", condition,
            "<br><b>Percent:</b> ", value, "%"
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            title         = list(text = paste("Soybean Conditions in", year_inner)),
            xaxis         = list(title = "Month", tickformat = "%b"),
            yaxis         = list(title = "Percent", range = c(0, 100)),
            legend        = list(title = list(text = "Condition")),
            plot_bgcolor  = "#F3E5D0",
            paper_bgcolor = "#F3E5D0"
          )
      })
    })
  }
}
