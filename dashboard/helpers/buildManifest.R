library(jsonlite)

# Step 1 — Generate the default manifest
rsconnect::writeManifest(appDir = "~/Desktop/grain/dashboard")

# Step 2 — Read it in
manifest_path <- file.path("~/Desktop/grain/dashboard", "manifest.json")
manifest <- jsonlite::fromJSON(manifest_path)

# Step 3 — If "packages" is a list instead of a data frame, convert it
if (is.list(manifest$packages) && !is.data.frame(manifest$packages)) {
  pkg_names <- names(manifest$packages)
  manifest$packages <- data.frame(
    Package = pkg_names,
    stringsAsFactors = FALSE
  )
}

# Step 4 — Keep only your actual packages
keep_pkgs <- c(
  "shiny", "tidyr", "rnassqs", "leaflet", "dplyr",
  "sf", "tigris", "plotly", "shinycssloaders",
  "kableExtra", "ggplot2", "lubridate"
)

manifest$packages <- manifest$packages[
  manifest$packages$Package %in% keep_pkgs, , drop = FALSE
]

# Step 5 — Write the cleaned manifest back
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

cat("✅  Custom manifest.json written successfully — terra excluded!\n")

