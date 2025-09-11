# ====================================================================
# app.R
# ====================================================================
library(shiny)
library(rnassqs)
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")
source("functions.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

