#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Combining GHCN Station-Level data with ESAC
#'  DEPENDENCIES:
#'  - Must have run:
#'      + Data_ESACCI-Biomass.r 
#'      + Data_CHELSA.r
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
Dir.CHELSA <- "/div/no-backup-nac/PATHFINDER/CHELSA"
Dir.DEM <- "/div/no-backup-nac/PATHFINDER/EU-DEM"
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"
if(!dir.exists(Dir.EmulatorData)){dir.create(Dir.EmulatorData)}

# DATA ====================================================================
## Loading ----------------------------------------------------------------
ESA_agb_rast <- rast(file.path(Dir.ESA, "ESA-BIOMASS_1km_2015-2022.nc"))[[1:5]] # don't need to keep the SD layers, and also cannot use 2020-onwards data as that is not matched by CHELSA
GISCO_DEM_rast <- rast(file.path(Dir.DEM, "GISCO_DEM.nc"))
### minor dicrepancies between the "1km resolutions" of the two data sets necessitate resampling from finer to coarser, also can shave off all data predating 2015
CHELSA_TAVG_rast <- resample(rast(file.path(Dir.CHELSA, "CHELSA_tas2000-2019_MONTHLY.nc"))[[(12*15+1):240]], ESA_agb_rast)
CHELSA_TMIN_rast <- resample(rast(file.path(Dir.CHELSA, "CHELSA_tasmin2000-2019_MONTHLY.nc"))[[(12*15+1):240]], ESA_agb_rast)
CHELSA_TMAX_rast <- resample(rast(file.path(Dir.CHELSA, "CHELSA_tasmax2000-2019_MONTHLY.nc"))[[(12*15+1):240]], ESA_agb_rast)

## Combining --------------------------------------------------------------
Extract_ls <- pblapply(1:nlyr(CHELSA_TAVG_rast), FUN = function(Iter){
    # Iter = 1
    # print(Iter)
    ### obtain relevant layers
    TAVG_Iter <- CHELSA_TAVG_rast[[Iter]]
    TMIN_Iter <- CHELSA_TMIN_rast[[Iter]]
    TMAX_Iter <- CHELSA_TMAX_rast[[Iter]]

    ### make output data frame
    Base_df <- as.data.frame(TAVG_Iter, cells = TRUE, time = TRUE, na.rm = FALSE, xy = TRUE)
    colnames(Base_df) <- c("CELL", "LONGITUDE", "LATITUDE", "mean")
    Base_df$YEAR_MONTH <- substr(time(TAVG_Iter), 1, 7)
    Base_df <- Base_df[,c(1:3,5, 4)]
    Base_df$min <- as.data.frame(TMIN_Iter, na.rm = FALSE)[,1]
    Base_df$max <- as.data.frame(TMAX_Iter, na.rm = FALSE)[,1]
    Base_df$ELEVATION <- as.data.frame(GISCO_DEM_rast, na.rm = FALSE)[,1]

    ### add AGB data
    ESALyr <- which(substr(time(ESA_agb_rast), 1, 4) == substr(Base_df$YEAR_MONTH[1], 1, 4))
    if(length(ESALyr) == 0){
        ESAVal <- NA
    }else{
        # Convert the LAT and LON to a matrix of coordinates
        coords <- as.matrix(Base_df[, c("LONGITUDE", "LATITUDE")])  # order: lon, lat
        # Extract raster values for those coordinates
        ESAVal <- extract(
            ESA_agb_rast[[ESALyr]],
            coords)[,1]
    }
    Base_df$AGB_ESA <- ESAVal
    Base_df
})

## Exporting --------------------------------------------------------------
Data_1km_df <- do.call(rbind, Extract_ls)
## chelsa temp data is stored multiplied by 10 and in Kelvin
Data_1km_df$mean <- Data_1km_df$mean/10-272.15
Data_1km_df$min <- Data_1km_df$min/10-272.15
Data_1km_df$max <- Data_1km_df$max/10-272.15
# write.csv(Data_1km_df, "Data_1km_df.csv")
saveRDS(Data_1km_df, file.path(Dir.EmulatorData, "Data_1km_df.rds"))