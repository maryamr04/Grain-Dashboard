# app.R
library(shiny)
library(rnassqs)
source("functions.R")
source("ui.R")
source("server.R")
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")
shinyApp(ui = ui, server = server)

