# ====================================================================
# app.R
# ====================================================================
source("functions.R")
library(rnassqs)
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")
source("server.R")
source("ui.R")

library(shiny)
library(tidyr)
library(leaflet)
library(dplyr)
library(sf)      
shinyApp(ui = ui, server = server)


