#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Combining CERRA data with ESAC
#'  DEPENDENCIES:
#'  - Must have run:
#'      + Data_ESACCI-Biomass.r
#'      + Data_CERRA.r
#'      + Data_DEM.r
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
Dir.ESA <- "/div/no-backup-nac/PATHFINDER/ESACCI-BIOMASS"
Dir.CERRA <- "/div/no-backup-nac/PATHFINDER/CERRA"
Dir.DEM <- "/div/no-backup-nac/PATHFINDER/EU-DEM"
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"
if (!dir.exists(Dir.EmulatorData)) {
    dir.create(Dir.EmulatorData)
}

# DATA ====================================================================
## Loading ----------------------------------------------------------------
### CERRA data
CERRA_mean <- rast(file.path(Dir.CERRA, "CERRA_mean.nc"))
CERRA_min <- rast(file.path(Dir.CERRA, "CERRA_min.nc"))
CERRA_max <- rast(file.path(Dir.CERRA, "CERRA_max.nc"))
time(CERRA_mean) <- time(CERRA_min) <- time(CERRA_max) <- as.POSIXct(time(CERRA_mean), tz = "CET")

### data to match
ESA_agb_rast <- rast(file.path(Dir.ESA, "ESA-BIOMASS_1km_2015-2022.nc"))[[1:8]] # don't need to keep the SD layers
ESA_agb_rast <- resample(ESA_agb_rast, CERRA_mean)
GISCO_DEM_rast <- rast(file.path(Dir.DEM, "GISCO_DEM.nc"))
GISCO_DEM_rast <- resample(GISCO_DEM_rast, CERRA_mean)

### reduce CERRA data to matchable time range
CERRA_mean <- CERRA_mean[[which(substr(time(CERRA_mean), 1, 4) %in% substr(time(ESA_agb_rast), 1, 4))]]
CERRA_min <- CERRA_min[[which(substr(time(CERRA_min), 1, 4) %in% substr(time(ESA_agb_rast), 1, 4))]]
CERRA_max <- CERRA_max[[which(substr(time(CERRA_max), 1, 4) %in% substr(time(ESA_agb_rast), 1, 4))]]


## Combining --------------------------------------------------------------
Extract_ls <- pblapply(1:nlyr(CERRA_mean), FUN = function(Iter) {
    # Iter = 1
    # print(Iter)
    ### obtain relevant layers
    TAVG_Iter <- CERRA_mean[[Iter]]
    TMIN_Iter <- CERRA_min[[Iter]]
    TMAX_Iter <- CERRA_max[[Iter]]

    ### make output data frame
    Base_df <- as.data.frame(TAVG_Iter, cells = TRUE, time = TRUE, na.rm = FALSE, xy = TRUE)
    colnames(Base_df) <- c("CELL", "LONGITUDE", "LATITUDE", "mean")
    Base_df$YEAR_MONTH <- substr(time(TAVG_Iter), 1, 7)
    Base_df <- Base_df[, c(1:3, 5, 4)]
    Base_df$min <- as.data.frame(TMIN_Iter, na.rm = FALSE)[, 1]
    Base_df$max <- as.data.frame(TMAX_Iter, na.rm = FALSE)[, 1]
    Base_df$ELEVATION <- as.data.frame(GISCO_DEM_rast, na.rm = FALSE)[, 1]

    ### add AGB data
    ESALyr <- which(substr(time(ESA_agb_rast), 1, 4) == substr(Base_df$YEAR_MONTH[1], 1, 4))
    if (length(ESALyr) == 0) {
        ESAVal <- NA
    } else {
        # Convert the LAT and LON to a matrix of coordinates
        coords <- as.matrix(Base_df[, c("LONGITUDE", "LATITUDE")]) # order: lon, lat
        # Extract raster values for those coordinates
        ESAVal <- extract(
            ESA_agb_rast[[ESALyr]],
            coords
        )[, 1]
    }
    Base_df$AGB_ESA <- ESAVal
    Base_df
})

## Exporting --------------------------------------------------------------
Data_5km_df <- do.call(rbind, Extract_ls)
# write.csv(Data_1km_df, "Data_1km_df.csv")
saveRDS(Data_5km_df, file.path(Dir.EmulatorData, "Data_5km_df.rds"))
