#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - DEM data
#'      + Download
#'      + Crop
#'  DEPENDENCIES:
#'  - Must have run:
#'      + Data_ESACCI-Biomass.r
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
Dir.ESA <- file.path(Dir.Base, "ESACCI-BIOMASS") # "/div/no-backup-nac/PATHFINDER/ESACCI-BIOMASS"
Dir.DEM <- file.path(Dir.Base, "EU-DEM")
if (!dir.exists(Dir.DEM)) {
    dir.create(Dir.DEM)
}

# DATA ====================================================================
## Loading ----------------------------------------------------------------
ESA_agb_rast <- rast(file.path(Dir.ESA, "ESA-BIOMASS_1km_2015-2022.nc"))

## Download --------------------------------------------------------
FileName <- "GISCO_DEM.nc"
Meta_vec <- c(DOI = "", CITATION = "European Environment Agency. EU-DEM v1.1. 2021. Dataset. European Environment Agency, https://sdi.eea.europa.eu/catalogue/srv/eng/catalog.search#/metadata/d08852bc-7b5f-4835-a776-08362e2fbf4b.", Project = "PATHFINDER", Handler = "Erik Kusch")

FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir.DEM, loadFun = terra::rast, load = TRUE, verbose = TRUE)
if (!is.null(FCheck)) {
    FCheck <- ClimHub:::WriteRead.NC(NC = FCheck, FName = file.path(Dir.DEM, FileName), Attrs = Meta_vec)
    return(FCheck)
} else {
    ### download ------
    options(timeout = 3600)
    download.file(
        url = "https://gisco-services.ec.europa.eu/dem/5degree/mosaic/EU_DEM_mosaic_5deg.ZIP",
        destfile = file.path(Dir.DEM, "EU_DEM_mosaic_5deg.ZIP"),
        timeout = 3600
    )

    ### unzip ------
    unzip(file.path(Dir.DEM, "EU_DEM_mosaic_5deg.ZIP"), exdir = Dir.DEM)

    ### load ------
    EUDEM_rast <- rast(file.path(Dir.DEM, "eudem_dem_4258_europe.tif"))

    ### crop ------
    EUDEM_rast <- aggregate(EUDEM_rast, fact = res(ESA_agb_rast)[1] / res(EUDEM_rast)[1], fun = mean)
    # resample(EUDEM_rast, ESA_agb_rast)
    EUDEM_rast2 <- Spatial.Reproject(EUDEM_rast, 4326)
    EUDEM_rast3 <- Spatial.CropMask(EUDEM_rast2, terra::ext(c(-10, 30, 35, 70)))

    ### save ------
    time(EUDEM_rast3) <- as.POSIXct("2025-07-02")
    EUDEM_rast3 <- ClimHub:::WriteRead.NC(
        NC = EUDEM_rast3, FName = file.path(Dir.DEM, FileName),
        Variable = "Elevation",
        LongVar = "Elevation",
        Unit = "m",
        Attrs = Meta_vec, Write = TRUE, Compression = 9
    )

    unlink(c(
        file.path(Dir.DEM, "eudem_dem_4258_europe.tif"),
        file.path(Dir.DEM, "EU_DEM_mosaic_5deg.ZIP")
    ))

    EUDEM_rast3
}
