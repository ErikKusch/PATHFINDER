#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Combining GHCN Station-Level data with ESAC
#'  DEPENDENCIES:
#'  - Must have run:
#'      + Data_ESACCI-Biomass.r
#'      + Data_GHCN.r
#'  - Must be present:
#'      + EmulatorReadying.r
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
    "pbapply" # for progress bars with estimator
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
Dir.GHCN <- "/div/no-backup-nac/PATHFINDER/GHCN"
Dir.ESA <- "/div/no-backup-nac/PATHFINDER/ESACCI-BIOMASS"
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"
if(!dir.exists(Dir.EmulatorData)){dir.create(Dir.EmulatorData)}

## Functionality ------------------------------------------------------------
source("EmulatorReadying.r")

# DATA ====================================================================
## Loading ----------------------------------------------------------------
ESA_agb_rast <- rast(file.path(Dir.ESA, "ESA-BIOMASS_1km_2015-2022.nc"))
GHCN_df <- readRDS(file.path(Dir.GHCN, "GHCN_2000-2024_MONTHLY.rds"))
Stations_df <- readRDS(file.path(Dir.GHCN, "GHCN_Stations_2000-2024_CLEANED.rds"))

## Combining --------------------------------------------------------------
Extract_ls <- pblapply(1:nrow(GHCN_df), cl = 36, FUN = function(Iter){
    # print(Iter)
    Iter_df <- GHCN_df[Iter, ]
    Iter_df <- cbind(Stations_df[which(Stations_df$STATION == Iter_df$STATION), ], Iter_df[ , -1])
    ESALyr <- which(substr(time(ESA_agb_rast), 1, 4) == substr(Iter_df$YEAR_MONTH, 1, 4))
    if(length(ESALyr) == 0){
        ESAVal <- NA
    }else{
        # Convert the LAT and LON to a matrix of coordinates
        coords <- as.matrix(Iter_df[, c("LONGITUDE", "LATITUDE")])  # order: lon, lat
        # Extract raster values for those coordinates
        ESAVal <- extract(
            ESA_agb_rast[[ESALyr[1]]], # select first of the two layers as they are agb and agb_sd
            coords)
    }
    Iter_df$AGB_ESA <- ESAVal
    Iter_df
})
StationData_df <- do.call(rbind, Extract_ls)

## Adding Derived Information ---------------------------------------------
StationData_df <- EmulatorReadying(StationData_df)
write.csv(apply(StationData_df, 2, as.character), file.path(Dir.EmulatorData, "Data_StationLevel.csv"))
saveRDS(StationData_df, file.path(Dir.EmulatorData, "Data_StationLevel.rds"))