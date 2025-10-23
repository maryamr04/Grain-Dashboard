# ====================================================================
# server.R
# ====================================================================
source("functions.R")
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)      
library(tigris)  
options(tigris_use_cache = TRUE)

server <- function(input, output, session) {
  
  # ---------------------- Planting Progress --------------------------
  observe({
    for (cat in progress_categories) {
      local({
        category_inner <- cat
        output_id <- paste0("soy_progress_", gsub("[^A-Za-z]", "_", category_inner))
        
        output[[output_id]] <- renderPlotly({
          req(input$year_sel)
          
          if (input$year_sel %in% 2014:2024) {
            actual <- soy_progress %>%
              filter(Year == as.character(input$year_sel),
                     CategoryRaw == category_inner,
                     Type == "Actual") %>%
              select(Year, week, CategoryRaw, value, Type)
            
            avg <- soy_progress %>%
              filter(CategoryRaw == category_inner,
                     Type == "5-Year Avg") %>%
              transmute(Year, week, CategoryRaw,
                        value = five_year_avg_value,
                        Type)
            
            combined <- bind_rows(actual, avg)
          
          } else if (input$year_sel == 2025) {
            # For 2025, only API Actuals
            combined <- get_soy_progress_data(input$year_sel, category_inner)
          } else {
            combined <- NULL
          }
          
          # Handle missing data
          if (is.null(combined) || nrow(combined) == 0) {
            return(
              plotly_empty(type = "scatter", mode = "lines") %>%
                layout(title = list(
                  text = paste("No data for", clean_title(category_inner), "-", input$year_sel)
                ))
            )
          }
          
          # Plot
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
  output$county_map <- renderLeaflet({
    req(input$map_year_sel)
    
    # Filter soybean data for selected year
    df <- soy_county %>%
      filter(Year == as.character(input$map_year_sel)) %>%
      mutate(
        County = tolower(County),
        County = gsub(" county", "", County),
        County = trimws(County)
      )
    
    if (nrow(df) == 0) {
      return(
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          addLabelOnlyMarkers(
            lng = -78.6569, lat = 37.5,
            label = paste("No soybean data for", input$map_year_sel),
            labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
          ) %>%
          setView(lng = -78.6569, lat = 37.5, zoom = 6)
      )
    }

    map_data <- left_join(
      all_counties,
      df,  
      by = c("County", "State")
    ) %>% st_as_sf()

    
    pal <- colorBin(
      palette = c("#8B4513", "#DEB887", "#228B22"),  # earthy: brown → tan → green
      domain = map_data$SuccessRate,
      bins = 5,
      na.color = "#f0f0f0"
    )
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(SuccessRate),
        color = "white",
        weight = 0.6,
        fillOpacity = 0.8,
        label = ~paste0(
          "<strong>", tools::toTitleCase(County), ", ", State, "</strong><br>",
          "🌱 Success Rate: ", ifelse(is.na(SuccessRate), "N/A", paste0(SuccessRate, "%")), "<br>",
          "🌾 Planted: ", ifelse(is.na(Planted), "N/A", formatC(Planted, big.mark=",")), "<br>",
          "🌾 Harvested: ", ifelse(is.na(Harvested), "N/A", formatC(Harvested, big.mark=","))
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~SuccessRate,
        title = "🌾 Success Rate (%)",
        opacity = 1
      ) %>%
      setView(lng = -78.6569, lat = 37.5, zoom = 6)
  })
  
  
  # ====================================================================
  # Remote Sensing 
  # ====================================================================
  
  
  # ------------------------ EDVI Trends --------------------------
  # Update county choices dynamically when year changes
  observe({
    updateSelectInput(session, "edvi_county",
                      choices = unique(soy_edvi$county_name),
                      selected = head(unique(soy_edvi$county_name), 1))
  })
  
  # Line plot of EDVI by county and week
  output$edvi_plot <- renderPlotly({
    req(input$edvi_year, input$edvi_county)
    
    df <- soy_edvi %>%
      filter(year == input$edvi_year,
             county_name %in% input$edvi_county)
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = list(text = paste("No EDVI data for", input$edvi_year))))
    }
    
    gg <- ggplot(df, aes(x = week, y = mean_EDVI,
                         color = county_name, group = county_name)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(
        values = c(
          "#2E7D32", # dark green
          "#66BB6A", # medium green
          "#A5D6A7", # light green
          "#8B4513", # saddle brown
          "#A0522D", # sienna
          "#CD853F", # peru
          "#D2B48C"  # tan
        )
      ) +
      labs(
        title = paste("Soybean EDVI Trends —", input$edvi_year),
        x = "Week of Year",
        y = "Mean EDVI"
      ) +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background  = element_rect(fill = "#F3E5D0", color = NA),
        legend.position  = "bottom"
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # Update county choices dynamically for EDVI
  observe({
    updateSelectInput(session, "edvi_county",
                      choices = unique(soy_edvi$county_name),
                      selected = head(unique(soy_edvi$county_name), 1))
  })
  
  # Update county choices dynamically for NDVI
  observe({
    updateSelectInput(session, "ndvi_county",
                      choices = unique(soy_edvi$county_name),
                      selected = head(unique(soy_edvi$county_name), 1))
  })
  
  
  # NDVI plot
  output$ndvi_plot <- renderPlotly({
    req(input$ndvi_year, input$ndvi_county)
    
    df <- soy_edvi %>%   # assuming you add NDVI column to same csv
      filter(year == input$ndvi_year,
             county_name %in% input$ndvi_county)
    
    if (nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = list(text = paste("No NDVI data for", input$ndvi_year))))
    }
    
    gg <- ggplot(df, aes(x = week, y = NDVI,   # <-- use NDVI col
                         color = county_name, group = county_name)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(
        values = c(
          "#2E7D32", # dark green
          "#66BB6A", # medium green
          "#A5D6A7", # light green
          "#8B4513", # saddle brown
          "#A0522D", # sienna
          "#CD853F", # peru
          "#D2B48C"  # tan
        ) 
      ) +
      labs(
        title = paste("Soybean NDVI Trends —", input$ndvi_year),
        x = "Week of Year",
        y = "Mean NDVI"
      ) +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background  = element_rect(fill = "#F3E5D0", color = NA),
        legend.position  = "bottom"
      )
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # ====================================================================
  # Yield Forecasts (Server)
  # ====================================================================
  
  # 1️⃣ Conditions-only
  output$yield_forecast_conditions_plot <- renderPlotly({
    req(input$year_forecast_conditions)
    df <- make_forecasts_conditions(as.integer(input$year_forecast_conditions))
    if (is.null(df)) return(NULL)
    actual <- soy_annual %>% filter(Year == input$year_forecast_conditions) %>% pull(Yield)
    
    p <- ggplot(df, aes(x = Week, y = Forecast_Yield)) +
      geom_line(color = "#2E8B57", size = 1.2) +
      geom_point(color = "#228B22") +
      geom_hline(yintercept = actual, color = "#8B0000", linetype = "dashed") +
      labs(title = paste("Conditions-only Forecast —", input$year_forecast_conditions),
           subtitle = "Green = forecast | Red dashed = actual yield",
           x = "Week", y = "Yield (bu/acre)") +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background  = element_rect(fill = "#F3E5D0", color = NA),
        legend.position  = "bottom"
      )
    
    ggplotly(p)
  })
  output$yield_forecast_conditions_summary <- renderText({
    df <- make_forecasts_conditions(as.integer(input$year_forecast_conditions))
    if (is.null(df)) return("No data available")
    actual <- soy_annual %>% filter(Year == input$year_forecast_conditions) %>% pull(Yield)
    rmse <- sqrt(mean((df$Forecast_Yield - actual)^2, na.rm = TRUE))
    paste("RMSE (Conditions-only model):", round(rmse, 2), "bu/acre")
  })
  
  # 2️⃣ EDVI Timeseries (across years, replaces EDVI-only weekly)
  output$yield_forecast_edvi_only_plot <- renderPlotly({
    req(input$year_forecast_edvi)
    
    # Filter all data up to the selected year
    df_plot <- soy_annual %>% filter(Year <= input$year_forecast_edvi)
    
    p <- ggplot(df_plot, aes(x = Year)) +
      # Actual Yield
      geom_line(aes(y = Yield, color = "Actual Yield"), size = 1) +
      geom_point(aes(y = Yield, color = "Actual Yield")) +
      
      # Trend Yield
      geom_line(aes(y = Trend_Yield, color = "Trend Yield"),
                linetype = "dashed", size = 1) +
      
      # EDVI forecast
      geom_line(aes(y = Trend_Yield * (1 + Percent_Deviation/100),
                    color = "Forecast (EDVI)"),
                linetype = "dotted", size = 1) +
      geom_point(aes(y = Trend_Yield * (1 + Percent_Deviation/100),
                     color = "Forecast (EDVI)")) +
      
      # Optional: Good+Excellent forecast
      geom_line(aes(y = Trend_Yield * (1 + Percent_Deviation/100),
                    color = "Forecast (G+E)"),
                linetype = "dotted", size = 1, alpha = 0.7) +
      
      labs(
        title = paste("Soybean Yield Forecast (up to", input$year_forecast_edvi, ")"),
        y = "Yield (bushels per acre)",
        x = "Year",
        subtitle = "Black = Actual | Blue dashed = Trend | Green dotted = EDVI Forecast | Orange dotted = G+E Forecast"
      ) +
      scale_color_manual(values = c(
        "Actual Yield" = "black",
        "Trend Yield" = "darkgreen",
        "Forecast (EDVI)" = "lightgreen",
        "Forecast (G+E)" = "brown"
      )) +
    theme_minimal() +
    theme(
      plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "#F3E5D0", color = NA),
      plot.background  = element_rect(fill = "#F3E5D0", color = NA),
      legend.position  = "bottom"
    )
    
    ggplotly(p)
  })
  
  output$yield_forecast_edvi_only_summary <- renderText({
    df_sub <- soy_annual %>% filter(Year == input$year_forecast_edvi)
    if (nrow(df_sub) == 0) return("No data available")
    
    rmse <- sqrt(mean((df_sub$Trend_Yield * (1 + df_sub$Percent_Deviation/100) -
                         df_sub$Yield)^2, na.rm = TRUE))
    paste("RMSE (EDVI timeseries model):", round(rmse, 2), "bu/acre")
  })
  
  
  # 3️⃣ Conditions + EDVI
  output$yield_forecast_cond_edvi_plot <- renderPlotly({
    req(input$year_forecast_cond_edvi)
    df <- make_forecasts_cond_edvi(as.integer(input$year_forecast_cond_edvi))
    if (is.null(df)) return(NULL)
    actual <- soy_annual %>% filter(Year == input$year_forecast_cond_edvi) %>% pull(Yield)
    
    p <- ggplot(df, aes(x = Week, y = Forecast_Yield)) +
      geom_line(color = "#006400", size = 1.2) +
      geom_point(color = "#32CD32") +
      geom_hline(yintercept = actual, color = "#8B0000", linetype = "dashed") +
      labs(title = paste("Conditions + EDVI Forecast —", input$year_forecast_cond_edvi),
           subtitle = "Dark green = forecast | Red dashed = actual yield",
           x = "Week", y = "Yield (bu/acre)") +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background  = element_rect(fill = "#F3E5D0", color = NA),
        legend.position  = "bottom"
      )
    
    ggplotly(p)
  })
  output$yield_forecast_cond_edvi_summary <- renderText({
    df <- make_forecasts_cond_edvi(as.integer(input$year_forecast_cond_edvi))
    if (is.null(df)) return("No data available")
    actual <- soy_annual %>% filter(Year == input$year_forecast_cond_edvi) %>% pull(Yield)
    rmse <- sqrt(mean((df$Forecast_Yield - actual)^2, na.rm = TRUE))
    paste("RMSE (Conditions + EDVI model):", round(rmse, 2), "bu/acre")
  })
  
  # 4️⃣ ARIMAX Forecast
  output$yield_forecast_arimax_plot <- renderPlotly({
    df <- make_forecasts_arimax(start_year = 2018, end_year = max(soy_annual$Year))
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Actual, color = "Actual Yield"), size = 1.2) +
      geom_point(aes(y = Actual, color = "Actual Yield")) +
      
      geom_line(aes(y = Forecast, color = "ARIMAX Forecast"), size = 1.2) +
      geom_point(aes(y = Forecast, color = "ARIMAX Forecast"), size = 2) +
      
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightgreen", alpha = 0.3) +
      
      labs(
        title = "ARIMAX Forecasts (Year-by-Year)",
        subtitle = "Black = Actual | Green = Forecast + CI",
        x = "Year", y = "Yield (bu/acre)"
      ) +
      scale_color_manual(values = c(
        "Actual Yield" = "black",
        "ARIMAX Forecast" = "#228B22"
      )) +
      theme_minimal() +
      theme(
        plot.title       = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        plot.background  = element_rect(fill = "#F3E5D0", color = NA),
        legend.position  = "bottom"
      )
    
    ggplotly(p)
  })
  
  output$yield_forecast_arimax_summary <- renderText({
    df <- make_forecasts_arimax(start_year = 2018, end_year = max(soy_annual$Year))
    if (is.null(df) || nrow(df) == 0) return("No data available")
    
    rmse <- sqrt(mean((df$Forecast - df$Actual)^2, na.rm = TRUE))
    
    paste(
      "ARIMAX Model RMSE across years:", round(rmse, 2), "bu/acre"
    )
  })
  
  # ================================================================
  # --- Smart Model Comparison Plot (Yearly, model-specific years)
  # ================================================================
  output$yield_forecast_comparison_plot <- renderPlotly({
    
    # === Individual model forecasts (only for valid years) ===
    
    cond_df <- soy_annual %>%
      mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_conditions, newdata = ., allow.new.levels = TRUE)/100),
             Model = "Conditions Only") %>%
      select(Year, Forecast_Yield, Model)
    
    edvi_df <- soy_annual %>%
      mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_edvi_only, newdata = ., allow.new.levels = TRUE)/100),
             Model = "EDVI Only") %>%
      select(Year, Forecast_Yield, Model)
    
    hybrid_df <- soy_annual %>%
      mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_cond_edvi, newdata = ., allow.new.levels = TRUE)/100),
             Model = "Conditions + EDVI") %>%
      select(Year, Forecast_Yield, Model)
    
    arimax_df <- make_forecasts_arimax(start_year = 2018, end_year = max(soy_annual$Year)) %>%
      rename(Forecast_Yield = Forecast) %>%
      mutate(Model = "ARIMAX") %>%
      select(Year, Forecast_Yield, Model)
    
    actual_df <- soy_annual %>% 
      select(Year, Forecast_Yield = Yield) %>%
      mutate(Model = "Actual Yield")
    
    trend_df <- soy_annual %>%
      select(Year, Forecast_Yield = Trend_Yield) %>%
      mutate(Model = "Trend Yield")
    
    combined <- bind_rows(actual_df, trend_df, cond_df, edvi_df, hybrid_df, arimax_df)
    
    # 🌿 Nature-inspired color palette
    nature_colors <- c(
      "Actual Yield"      = "#2F3E46",  # dark soil green-gray
      "Trend Yield"       = "#6BA292",  # sage green
      "Conditions Only"   = "#A8B545",  # olive green
      "EDVI Only"         = "#5CA4A9",  # teal blue
      "Conditions + EDVI" = "#D6A75A",  # golden harvest
      "ARIMAX"            = "#8B4513"   # rich brown
    )
    
    # === Plot ===
    gg <- ggplot(combined, aes(x = Year, y = Forecast_Yield, color = Model, linetype = Model)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 2.5, na.rm = TRUE) +
      labs(
        title = "🌾 Soybean Yield Forecast Comparison (2014–2025)",
        subtitle = "Each model shown by year; all years spaced evenly across the timeline",
        x = "Year",
        y = "Yield (bushels per acre)"
      ) +
      scale_color_manual(values = nature_colors) +
      scale_linetype_manual(values = c(
        "Actual Yield" = "solid",
        "Trend Yield" = "dashed",
        "Conditions Only" = "solid",
        "EDVI Only" = "solid",
        "Conditions + EDVI" = "solid",
        "ARIMAX" = "solid"
      )) +
      # 👇 Force every year to display on x-axis
      scale_x_continuous(breaks = seq(2014, 2025, 1), expand = c(0.01, 0.01)) +
      theme_minimal(base_family = "Times New Roman") +
      theme(
        plot.background = element_rect(fill = "#F3E5D0", color = NA),
        panel.background = element_rect(fill = "#F3E5D0", color = NA),
        panel.grid.major = element_line(color = "#DCC7A1", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#4B2E2B"),
        axis.title = element_text(color = "#4B2E2B", face = "bold"),
        plot.title = element_text(color = "#3E2C23", face = "bold", size = 18),
        plot.subtitle = element_text(color = "#4B2E2B", size = 13),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "#4B2E2B")
      )
    
    ggplotly(gg) %>%
      layout(legend = list(orientation = "h", y = -0.25))
  })
  
  
  
  output$yield_forecast_comparison_summary <- renderText({
    df_arimax <- make_forecasts_arimax(start_year = 2018, end_year = max(soy_annual$Year))
    
    # Calculate RMSE using only overlapping years for fairness
    overlap <- function(pred, actual) {
      shared <- intersect(pred$Year, actual$Year)
      pred <- pred %>% filter(Year %in% shared)
      actual <- actual %>% filter(Year %in% shared)
      sqrt(mean((pred$Forecast_Yield - actual$Yield)^2, na.rm = TRUE))
    }
    
    actual <- soy_annual %>% select(Year, Yield)
    
    rmse_cond <- overlap(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_conditions, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    rmse_edvi <- overlap(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_edvi_only, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    rmse_hybrid <- overlap(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield * (1 + predict(reg_model_cond_edvi, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    rmse_arimax <- overlap(df_arimax %>% rename(Forecast_Yield = Forecast), actual)
    
    paste0(
      "Model RMSEs — Conditions: ", round(rmse_cond, 2),
      " | EDVI: ", round(rmse_edvi, 2),
      " | Hybrid: ", round(rmse_hybrid, 2),
      " | ARIMAX: ", round(rmse_arimax, 2)
    )
  })
  
  output$model_rmse_table <- renderUI({
    df_arimax <- make_forecasts_arimax(start_year = 2018, end_year = max(soy_annual$Year))
    
    overlap_rmse <- function(pred, actual) {
      shared <- intersect(pred$Year, actual$Year)
      pred <- pred %>% filter(Year %in% shared)
      actual <- actual %>% filter(Year %in% shared)
      sqrt(mean((pred$Forecast_Yield - actual$Yield)^2, na.rm = TRUE))
    }
    
    actual <- soy_annual %>% select(Year, Yield)
    
    rmse_cond <- overlap_rmse(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield *
                              (1 + predict(reg_model_conditions, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    
    rmse_edvi <- overlap_rmse(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield *
                              (1 + predict(reg_model_edvi_only, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    
    rmse_hybrid <- overlap_rmse(
      soy_annual %>% mutate(Forecast_Yield = Trend_Yield *
                              (1 + predict(reg_model_cond_edvi, newdata = ., allow.new.levels = TRUE)/100)),
      actual
    )
    
    rmse_arimax <- overlap_rmse(
      df_arimax %>% rename(Forecast_Yield = Forecast),
      actual
    )
    
    # Build and format the table
    df <- tibble::tibble(
      Model = c("Conditions Only", "EDVI Only", "Conditions + EDVI", "ARIMAX"),
      RMSE = round(c(rmse_cond, rmse_edvi, rmse_hybrid, rmse_arimax), 2)
    ) %>%
      arrange(RMSE)
    
    # 🌿 Styled dark-green & white table
    HTML(
      df %>%
        knitr::kable(format = "html", align = "c", col.names = c("Model", "RMSE (bu/acre)")) %>%
        kableExtra::kable_styling(
          full_width = FALSE,
          position = "center",
          bootstrap_options = c("striped", "hover", "condensed")
        ) %>%
        kableExtra::row_spec(0, background = "#004d00", color = "white", bold = TRUE) %>%    
        kableExtra::row_spec(1:nrow(df), background = "#e8f5e9", color = "black") %>%        
        kableExtra::column_spec(2, bold = TRUE, color = "#1b5e20")                        
    )
  })
  
    
    
  
  # ====================================================================
  # Assistant Bot?
  # ====================================================================
  
  # ---------------------- Assistant Helper --------------------------
  faq_answers <- list(
    "What does the forecast mean?" =
      "The forecast shows predicted soybean yields based on crop conditions (Good + Excellent) and historical yield trends.",
    
    "What do the dashed lines show?" =
      "Dashed lines represent the trend yield (baseline yield expected without unusual conditions).",
    
    "Where does the data come from?" =
      "All data comes from USDA NASS survey datasets and satellite-based remote sensing (NDVI, EDVI, MODIS temperature).",
    
    "How do I use the planting progress tab?" =
      "Select a year from 2014–2025 to compare weekly planting/development progress with the 5-year average.",
    
    "How do I interpret the county map?" =
      "Hover over each county to see acres planted, harvested, and harvest success rate. Darker colors mean higher values.",
    
    "What are NDVI and EDVI?" =
      "NDVI (Normalized Difference Vegetation Index) measures vegetation greenness. EDVI is an enhanced version that uses the blue band to reduce soil/atmosphere noise.",
    
    "What years are forecasts available for?" =
      "Forecasts are built for 2014–2024 (historical actual vs. forecast) and 2025 (live conditions, no actual yield yet)."
  )
  
  output$faq_answer <- renderText({
    req(input$faq_question)
    faq_answers[[input$faq_question]]
  })
  
  # ---------------- Feedback Handling ----------------
  observeEvent(input$submit_feedback, {
    req(input$user_feedback)
    
    save_feedback(input$user_name, input$user_feedback)
    
    output$feedback_message <- renderText("✅ Thanks for your feedback! It has been recorded.")
    
    updateTextInput(session, "user_name", value = "")
    updateTextAreaInput(session, "user_feedback", value = "")
  })
  
  
}

