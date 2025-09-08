# server.R
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  years <- 2020:2025
  categories <- c("PCT PLANTED", "PCT EMERGED", "PCT BLOOMING",
                  "PCT SETTING PODS", "PCT DROPPING LEAVES", "PCT HARVESTED")
  
  
  
  for (yr in years) {
    for (cat in categories) {
      safe_id <- paste0("soy_progress_", yr, "_", gsub("[^A-Za-z]", "_", cat))
      
      local({
        year_inner <- yr
        category_inner <- cat
        output_id <- safe_id
        
        output[[output_id]] <- renderPlotly({
          actual <- get_soy_progress_data(year_inner, category_inner)
          avg <- get_soy_avg_data(year_inner, category_inner)
          
          if ((is.null(actual) || nrow(actual) == 0) &&
              (is.null(avg) || nrow(avg) == 0)) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No data for",
                                                      clean_title(category_inner),
                                                      "-", year_inner))))
          }
          
          combined <- bind_rows(
            if (!is.null(actual)) mutate(actual, Type = "Actual") else NULL,
            if (!is.null(avg)) mutate(avg, Type = "5-Year Avg") else NULL
          )
          
          ggplotly(
            ggplot(combined, aes(x = week, y = value, color = Type)) +
              geom_line(linewidth = 1.2) +
              geom_point(size = 2.5) +
              scale_color_manual(values = c("Actual" = "#2E7D32", "5-Year Avg" = "#8D6E63")) +
              labs(title = paste(clean_title(category_inner), "—", year_inner),
                   x = "Week Ending", y = "%", color = "Legend") +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.background = element_rect(fill = "#F3E5D0", color = NA),
                    plot.background = element_rect(fill = "#F3E5D0", color = NA)),
            tooltip = c("x", "y", "color")
          )
        })
      })
    }
  }
}

