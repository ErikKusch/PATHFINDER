#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Overlap of ESA CCI Biomass an CORINE Land Cover
#'      + Load data
#'      + transform ESA CCI Biomass to binary representation of forest/non-forest
#'      + Calculate overlap with CORINE Land Cover forest classes
#'  DEPENDENCIES:
#'  - data in CORINE subdirectory (produced through manual download)
#'  - data in ESACCI-BIOMASS subdirectory (produced by DATA_ESACCI-Biomass.r)
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
    "terra" # for spatraster operations
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("ErikKusch/ClimHub")
}
library(ClimHub)
package_vec <- c("ClimHub", package_vec)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.ESA <- file.path(Dir.Base, "ESACCI-BIOMASS")
Dir.CORINE <- file.path(Dir.Base, "CORINE")

# DATA IMPORT AND PREPARATION =============================================
## Load ESA CCI Biomass data ----------------------------------------------
ESA_rast <- terra::rast(file.path(Dir.ESA, "ESA-BIOMASS_100m_2015-2022.nc"))
ESA_rast <- ESA_rast[[grep(terra::time(ESA_rast), pattern = "2020")[1]]] # no need to keep sd layer

## Load CORINE Land Cover data --------------------------------------------
CORINE_rast <- terra::rast(file.path(Dir.CORINE, "u2018_clc2018_v2020_20u1_raster100m", "DATA", "U2018_CLC2018_V2020_20u1.tif"))

## Reproject ESA CCI to CORINE Land Cover ---------------------------------
ESARepro_rast <- Spatial_Reproject(ESA_rast, CORINE_rast, rasterResample = TRUE)
