# app.R
# Purpose: App entry point. Loads dependencies and sources other files,

library(shiny)
source("ui.R", local = TRUE)

shinyApp(
  ui = ui,
  server = function(input, output, session) { }
)



