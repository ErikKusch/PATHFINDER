#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - ESACCI-Biomass data
#'      + Download
#'      + Crop
#'  DEPENDENCIES:
#'  - None
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
Dir.ESA <- "/div/no-backup-nac/PATHFINDER/ESACCI-BIOMASS"
if(!dir.exists(Dir.ESA)){dir.create(Dir.ESA)}

# DATA ====================================================================
## Download ---------------------------------------------------------------
Meta_vec <- c(DOI = "doi: 10.5285/95913ffb6467447ca72c4e9d8cf30501", CITATION = "Santoro, M.; Cartus, O. (2025): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2007, 2010, 2015, 2016, 2017, 2018, 2019, 2020, 2021 and 2022, v6.0. NERC EDS Centre for Environmental Data Analysis, 17 April 2025.", Project = "PATHFINDER", Handler = "Erik Kusch")

FileName <- paste0("ESA-BIOMASS_2007-2022.nc")
FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir.ESA, loadFun = terra::rast, load = TRUE, verbose = TRUE)

if (!is.null(FCheck)) {
    ESA_rast <- ClimHub:::WriteRead.NC(NC = FCheck, FName = file.path(Dir.ESA, FileName), Attrs = Meta_vec)
}else{
    URLS <- paste0("https://dap.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v6.0/netcdf/ESACCI-BIOMASS-L4-AGB-MERGED-100m-", 2015:2022,"-fv6.0.nc?download=1")
    FNames <- paste0("ESA_TEMP", 2015:2022, ".nc")

    FilestoLoad <- ClimHub:::Helper.DirectDownload(URLS = URLS, Names = FNames, Cores = 12, Dir = Dir.ESA)
    ESA_rast <- ClimHub:::Helper.LoadFiles(FilestoLoad)

    ## Cropping ---------------------------------------------------------------
    ESA_rast <- Spatial.CropMask(ESA_rast, terra::ext(c(-10, 30, 35, 70)))
    # time(ESA_rast) <- dates

    ## Saving -----------------------------------------------------------------
    ESA_rast <- ClimHub:::WriteRead.NC(
        NC = ESA_rast, FName = file.path(Dir.ESA, FileName),
        Variable = "AGB",
        LongVar = "Above-Ground Biomass",
        Unit = "tons/ha",
        Attrs = Meta_vec, Write = TRUE, Compression = 9
    )

    unlink(file.path(Dir.ESA, FNames))
    }

ESA_rast