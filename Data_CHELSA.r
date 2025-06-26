#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - CHELSA data
#'      + Download
#'      + Crop
#'  - [ToDo:] - match with ESA BIOMASS data, make data frames of LAT, LON, TEMP AVG, TEMP Range, TEMP Max, TEMP Min, ELEVATION (obtain separate DEM or do we already have one?), Year, Month
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
Dir.CHELSA <- "/div/no-backup-nac/PATHFINDER/CHELSA"
if(!dir.exists(Dir.CHELSA)){dir.create(Dir.CHELSA)}

# DATA ====================================================================
Chelsa_vars <- c("tasmax", "tasmin", "tas")

## CHELSA Download --------------------------------------------------------
CHELSA_ls <- lapply(Chelsa_vars, FUN = function(VarIter) {
    # VarIter <- Chelsa_vars[1]
    print(VarIter)
    Meta_vec <- c(DOI = "doi: 10.1038/sdata.2017.122", CITATION = "Karger, D. N. et al. Climatologies at high resolution for the earth’s land surface areas. Sci. Data 4:170122 doi: 10.1038/sdata.2017.122 (2017).", Project = "PATHFINDER", Handler = "Erik Kusch")

    FileName <- paste0("CHELSA_", VarIter, "2000-2019_MONTHLY.nc")

    FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir.CHELSA, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        FCheck <- ClimHub:::WriteRead.NC(NC = FCheck, FName = file.path(Dir.CHELSA, FileName), Attrs = Meta_vec)
        return(FCheck)
    }else{
        # Generate sequence of monthly dates
    dates <- seq(from = as.Date("2000-01-01"), to = as.Date("2019-12-01"), by = "month")

    # Format as "MM_YYYY"
    month_year_vec <- format(dates, "%m_%Y")

    URLS <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/", VarIter, "/CHELSA_", VarIter, "_", month_year_vec, "_V.2.1.tif")
    FNames <- paste0("CHELSA_", VarIter, "_", month_year_vec, "_V.2.1.tif")

    FilestoLoad <- ClimHub:::Helper.DirectDownload(URLS = URLS, Names = FNames, Cores = 12, Dir = Dir.CHELSA)
    CHELSA_rast <- ClimHub:::Helper.LoadFiles(FilestoLoad)

    CHELSA_rast <- Spatial.CropMask(CHELSA_rast, terra::ext(c(-10, 30, 35, 70)))
    time(CHELSA_rast) <- dates

    CHELSA_rast <- ClimHub:::WriteRead.NC(
        NC = CHELSA_rast, FName = file.path(Dir.CHELSA, FileName),
        Variable = VarIter,
        LongVar = VarIter,
        Unit = unique(terra::units(CHELSA_rast)),
        Attrs = Meta_vec, Write = TRUE, Compression = 9
    )

    unlink(file.path(Dir.CHELSA, FNames))

    CHELSA_rast
    }
})
