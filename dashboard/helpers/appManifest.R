rsconnect::writeManifest(
  appDir = "~/Desktop/grain/dashboard",
  appPrimaryDoc = "app.R",
  renderArgs = list(quiet = TRUE),
  appDependencies = data.frame(
    Package = c(
      "shiny", "tidyr", "rnassqs", "leaflet", "dplyr",
      "sf", "tigris", "plotly", "shinycssloaders",
      "kableExtra", "ggplot2", "lubridate"
    ),
    stringsAsFactors = FALSE
  )
)
