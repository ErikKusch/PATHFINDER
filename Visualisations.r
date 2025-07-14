#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Visualisation of Data and Results
#'  DEPENDENCIES:
#'  - Data files directly in working directory (to be executed on personal machine)
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE ================================================================
rm(list = ls()) # some may not like it, but it helps my workflow

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, repos = "http://cran.us.r-project.org")
    }
    require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
    "terra", # for spatraster operations
    "sf", # for polygon handling
    "ggplot2", # for plotting
    "rnaturalearth" # four outlines of countries
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("ErikKusch/ClimHub")
}
library(ClimHub)
package_vec <- c("ClimHub", package_vec)

if ("KrigR" %in% rownames(installed.packages()) == FALSE) { # KrigR check
    devtools::install_github("ErikKusch/KrigR")
}
library(KrigR)
package_vec <- c("KrigR", package_vec)

# POLYGONS OF NATIONS =====================================================
Countries_sf <- ne_countries(scale = "medium")
Countries_sf <- st_crop(Countries_sf[, "admin"], xmin = -10, xmax = 30, ymin = 35, ymax = 70) # same as for the other data

# STATION-LEVEL DATA ======================================================
Stations_df <- readRDS("Data_StationLevel.rds")

## Station Locations ------------------------------------------------------
plot_df <- Stations_df[!duplicated(Stations_df$STATION), ]

Stations_gg <- ggplot() +
    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
    geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = STATION), col = "#ce2ad4", shape = 4) +
    theme_bw() +
    labs(title = paste0("GHCN Data Available for Emulator Computation after Quality Assessment (n = ", nrow(plot_df), ")"))
ggsave(Stations_gg, file = "GHCN_StationLocations.png", width = 16 * 2, height = 16 * 2, unit = "cm")

## Temperature Data -------------------------------------------------------
Stations_df$MONTH <- substr(Stations_df$YEAR_MONTH, 6, 7)
plot_df <- aggregate(mean ~ LONGITUDE + LATITUDE + MONTH + VARIABLE, data = Stations_df, FUN = mean, na.rm = TRUE)
plot_df$VARIABLE <- factor(plot_df$VARIABLE, levels = c("TMIN", "TAVG", "TMAX"))

TemperatureStations_gg <- ggplot() +
    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
    geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = mean)) +
    facet_grid(MONTH ~ VARIABLE) +
    # scale_colour_viridis_c() +  # does not work because colour is defined in points
    scale_colour_gradient2(
        low = "darkblue", # For negative values
        mid = "lightgrey", # For zero
        high = "darkred", # For positive values
        midpoint = 0 # Ensures the color diverges around 0
    ) +
    theme_bw() +
    labs(title = "GHCN Monthly Temperature Data", col = "")
ggsave(TemperatureStations_gg, file = "GHCN_MonthlyTemperatures.png", width = 16 * 2, height = 16 * 10, unit = "cm", limitsize = FALSE) # ToDo: TAVG data is not available for most stations - why?! Need to investigate

## Forest Cover Data ------------------------------------------------------
Stations_df$YEAR <- substr(Stations_df$YEAR_MONTH, 1, 4)
Stations_df$AGB_ESA <- unlist(Stations_df$AGB_ESA) # ToDo: need to fix this when preparing data!
plot_df <- aggregate(AGB_ESA ~ LONGITUDE + LATITUDE + YEAR, data = Stations_df, FUN = mean, na.rm = TRUE)

AGBStations_gg <- ggplot() +
    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
    geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = log(AGB_ESA))) +
    facet_wrap(~YEAR, ncol = 4) +
    # scale_colour_viridis_c() +  # does not work because colour is defined in points
    scale_colour_gradient(
        low = "lightgrey",
        high = "darkgreen",
        na.value = "black"
    ) +
    theme_bw() +
    labs(title = "Logarithmic GHCN Annual ESA AGB Data (Black Points are NA locations)", col = "")
ggsave(AGBStations_gg, file = "GHCN_Annual_ESA-AGB.png", width = 16 * 4, height = 9 * 4, unit = "cm", limitsize = FALSE)

# 1km RESOLUTION DATA =====================================================

## Elevation Data ---------------------------------------------------------

## CHELSA Data ------------------------------------------------------------

## Forest Cover Data ------------------------------------------------------

## Combined Data ----------------------------------------------------------
