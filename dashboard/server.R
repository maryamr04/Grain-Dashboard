# ====================================================================
# server.R
# ====================================================================
source("functions.R")
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  
  # ---------------------- Planting Progress --------------------------
  # Loop over categories so each one gets its own plot
  observe({
    for (cat in progress_categories) {
      local({
        category_inner <- cat
        output_id <- paste0("soy_progress_", gsub("[^A-Za-z]", "_", category_inner))
        
        output[[output_id]] <- renderPlotly({
          req(input$year_sel)
          
          # Get actual + 5-year average
          actual <- get_soy_progress_data(input$year_sel, category_inner)
          avg    <- get_soy_avg_data(input$year_sel, category_inner)
          
          combined <- bind_rows(
            if (!is.null(actual) && nrow(actual) > 0) actual,
            if (!is.null(avg) && nrow(avg) > 0) avg
          )
          
          # Handle missing data
          if (is.null(combined) || nrow(combined) == 0) {
            return(
              plotly_empty(type = "scatter", mode = "lines") %>%
                layout(title = list(text = paste("No data for",
                                                 clean_title(category_inner),
                                                 "-", input$year_sel)))
            )
          }
          
          # Plot with ggplot
          gg <- ggplot(combined, aes(x = week, y = value, color = Type)) +
            geom_line(linewidth = 1.2) +
            geom_point(size = 2.5) +
            scale_color_manual(
              values = c("Actual" = "#2E7D32", "5-Year Avg" = "#8D6E63"),
              name   = "Legend"
            ) +
            scale_x_date(date_labels = "%b", date_breaks = "1 month",
                         expand = c(0.01, 0.01)) +
            labs(
              title = paste(clean_title(category_inner), "—", input$year_sel),
              x = "Month",
              y = "Percent"
            ) +
            theme_minimal() +
            theme(
              plot.title      = element_text(size = 14, face = "bold", color = "#2E7D32"),
              axis.text.x     = element_text(angle = 45, hjust = 1),
              panel.background= element_rect(fill = "#F3E5D0", color = NA),
              plot.background = element_rect(fill = "#F3E5D0", color = NA)
            )
          
          ggplotly(gg, tooltip = c("x", "y", "color"))
        })
      })
    }
  })
  
  # ------------------------ Crop Conditions --------------------------
  output$plot_conditions <- renderPlotly({
    req(input$year_sel_conditions)
    
    df <- get_soybean_conditions(input$year_sel_conditions)
    
    if (is.null(df) || nrow(df) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "lines") %>%
          layout(title = list(text = paste("No data for Soybean Conditions —",
                                           input$year_sel_conditions)))
      )
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
        title         = list(text = paste("Soybean Conditions in", input$year_sel_conditions)),
        xaxis         = list(title = "Month", tickformat = "%b"),
        yaxis         = list(title = "Percent", range = c(0, 100)),
        legend        = list(title = list(text = "Condition")),
        plot_bgcolor  = "#F3E5D0",
        paper_bgcolor = "#F3E5D0"
      )
  })
  
  # ====================================================================
  # County Analysis
  # ====================================================================
  
  # ---- Acres Planted ----
  output$county_planted_plot <- renderPlotly({
    req(input$state_sel, input$county_sel)
    
    df <- soy_county %>%
      filter(State == input$state_sel,
             County %in% input$county_sel)
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = "No data for Acres Planted"))
    }
    
    gg <- ggplot(df, aes(x = as.numeric(Year), y = Planted,
                         color = County, group = County)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = rep("#2E7D32", length(unique(df$County)))) +  
      labs(
        title = paste("Soybean Acres Planted —", input$state_sel),
        x = "Year",
        y = "Acres Planted"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background = element_rect(fill = "#F3E5D0", color = NA)
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # ---- Acres Harvested ----
  output$county_harvested_plot <- renderPlotly({
    req(input$state_sel, input$county_sel)
    
    df <- soy_county %>%
      filter(State == input$state_sel,
             County %in% input$county_sel)
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = "No data for Acres Harvested"))
    }
    
    gg <- ggplot(df, aes(x = as.numeric(Year), y = Harvested,
                         color = County, group = County)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = rep("#2E7D32", length(unique(df$County)))) +  
      labs(
        title = paste("Soybean Acres Harvested —", input$state_sel),
        x = "Year",
        y = "Acres Harvested"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background = element_rect(fill = "#F3E5D0", color = NA)
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # ---- Harvest Success Rate ----
  output$county_success_plot <- renderPlotly({
    req(input$state_sel, input$county_sel)
    
    df <- soy_county %>%
      filter(State == input$state_sel,
             County %in% input$county_sel)
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = "No data for Harvest Success Rate"))
    }
    
    gg <- ggplot(df, aes(x = as.numeric(Year), y = SuccessRate,
                         color = County, group = County)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = rep("#2E7D32", length(unique(df$County)))) +  
      labs(
        title = paste("Soybean Harvest Success Rate —", input$state_sel),
        x = "Year",
        y = "Success Rate (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background = element_rect(fill = "#F3E5D0", color = NA)
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # ---------------- County Choropleth ----------------
output$county_map <- renderPlotly({
  req(input$map_year_sel)
  
  df <- soy_county %>%
    filter(Year == as.character(input$map_year_sel))   
  
  if (nrow(df) == 0) {
    return(plotly_empty(type = "choropleth") %>%
             layout(title = list(text = paste("No data for", input$map_year_sel))))
  }
  
  plot_ly(
    data = df,
    type = "choropleth",
    locations = ~GEOID,               
    geojson = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json",
    z = ~SuccessRate,
    colorscale = list(
      c(0, "rgb(139, 69, 19)"),       
      c(0.5, "rgb(222, 184, 135)"),  
      c(1, "rgb(34, 139, 34)")        
    ),
    marker = list(line = list(width = 0.3, color = "gray")),
    hoverinfo = "text",
    text = ~paste0(
      County, ", ", State, "<br>",
      "Harvest Success Rate: ", round(SuccessRate, 1), "%<br>",
      "Planted: ", Planted, "<br>",
      "Harvested: ", Harvested
    ),
    colorbar = list(
      title = list(text = "🌾 Success Rate (%)"),
      titleside = "top",
      tickformat = ".0f",
      len = 0.6,
      yanchor = "middle",
      thickness = 15
    )
    
  ) %>%
    layout(
      geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        fitbounds = "locations",
        bgcolor = "#F3E5D0"   # earthy tan background
      ),
      title = list(
        text = paste("Soybean Harvest Success Rate —", input$map_year_sel),
        font = list(family = "Times New Roman", size = 18, color = "#2E7D32")
      ),
      paper_bgcolor = "#F3E5D0"
    )
  
})

  
  
}




