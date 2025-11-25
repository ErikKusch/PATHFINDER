#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Combining CERRA data with ESAC
#'  DEPENDENCIES:
#'  - Must have run:
#'      + Data_ESACCI-Biomass.r
#'      + Data_CERRA.r
#'      + Data_DEM.r
#'  - Must have been manually downloaded:
#'      + CORINE data in CORINE directory
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
    "pbapply", # for progress bars with estimator
    "dplyr"
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.CORINE <- file.path(Dir.Base, "CORINE")
Dir.ESA <- file.path(Dir.Base, "ESACCI-BIOMASS")
Dir.CERRA <- file.path(Dir.Base, "CERRA")
Dir.DEM <- file.path(Dir.Base, "EU-DEM")
Dir.EmulatorData <- file.path(Dir.Base, "EmulatorData")
Dir.EmulatorDataCERRA <- file.path(Dir.Base, "CERRA")
if (!dir.exists(Dir.EmulatorDataCERRA)) {
    dir.create(Dir.EmulatorDataCERRA)
}

## Functionality ------------------------------------------------------------
EmulatorReadying <- function(RawData_df) {
    print("Adding derived information for emulator purposes")
    # Seasons defined in-line with doi:10.1029/2023EF004220; MAM:March‐April‐May; JJA: June‐July‐August; SON: September‐October‐November; and DJF: December‐January‐February
    RawData_df$SEASON <- pbsapply(RawData_df$YEAR_MONTH, FUN = function(x) {
        x <- substr(x, 6, 7)
        # print(x)
        if (x %in% c("03", "04", "05")) {
            Ret <- "MAM"
        }
        if (x %in% c("06", "07", "08")) {
            Ret <- "JJA"
        }
        if (x %in% c("09", "10", "11")) {
            Ret <- "SON"
        }
        if (x %in% c("12", "01", "02")) {
            Ret <- "DJF"
        }
        Ret
    })

    # Regions defined in-line with doi:10.1088/1748-9326/9/3/034002; might be better served with an overview of whether an area is predominantly boreal or deciduous forest; at present: everything is "North"
    RawData_df$REGION <- pbsapply(RawData_df$LATITUDE, FUN = function(x) {
        ifelse(x > 35, "North", "South")
    })

    # Range between min and max to get a better understanding of temperature fluctuations
    RawData_df$range <- RawData_df$max - RawData_df$min

    # Returning data
    return(RawData_df)
}

# DATA ====================================================================
## Loading ----------------------------------------------------------------
message("Loading Data")
### CERRA data -----
CERRA_fs <- list.files(path = Dir.CERRA, pattern = "max.nc|mean.nc|min.nc", full.names = TRUE)
CERRA_ls <- lapply(CERRA_fs, FUN = function(x) {
    CERRA_iter <- rast(x)
    time(CERRA_iter) <- as.POSIXct(time(CERRA_iter), tz = "CET")
    CERRA_iter
})
names(CERRA_ls) <- basename(CERRA_fs)

### AGB Data ------
ESA_agb_rast <- rast(file.path(Dir.EmulatorData, "AGB_Aligned.nc"))

### DEM Data ------
GISCO_DEM_rast <- rast(file.path(Dir.DEM, "GISCO_DEM.nc"))

### CORINE Data ------
CORINE_rast <- terra::rast(file.path(Dir.EmulatorData, "CORINE_Aligned.nc"))

## Combining --------------------------------------------------------------
message("Combining Data")
### Spatially limit to same extent -----
Exts <- list(
    terra::ext(CERRA_ls[[1]][[1]]),
    terra::ext(ESA_agb_rast),
    terra::ext(CORINE_rast),
    terra::ext(GISCO_DEM_rast)
)

MinimumOverlap <- terra::ext(
    c(
        max(unlist(lapply(Exts, "[", 1))),
        min(unlist(lapply(Exts, "[", 2))),
        max(unlist(lapply(Exts, "[", 3))),
        min(unlist(lapply(Exts, "[", 4)))
    )
)

print("### Cropping to shared extent")
CERRA_ls <- lapply(CERRA_ls, FUN = function(x){
    crop(x, MinimumOverlap)
})
ESA_agb_rast <- crop(ESA_agb_rast, MinimumOverlap)
CORINE_rast <- crop(CORINE_rast[[1]], MinimumOverlap)
GISCO_DEM_rast <- crop(GISCO_DEM_rast, MinimumOverlap)

### Regridding 100m data to fit CERRA resolution -----
print("### Regridding 100m data to fit CERRA resolution")
# Zones defined by CERRA data
polys <- as.polygons(CERRA_ls[[1]][[1]], dissolve = FALSE)

##### Mean / SD of ESA AGB and GISCO DEM ------
print("DEM and AGB data")
ContiStack_ls <- pblapply(list(DEM = GISCO_DEM_rast, AGB = ESA_agb_rast), FUN = function(x){
    # x <- ESA_agb_rast; x <- GISCO_DEM_rast
    
    # extracting by polygons
    zmean <- extract(x, polys, fun = mean, na.rm = TRUE)
    # head(zmean)
    zsd <- extract(x, polys, fun = sd, na.rm = TRUE)
    # head(zsd)

    # Extract numeric vectors
    v_mean <- zmean[,-1]
    v_sd   <- zsd[,-1]

    # force to be data frame to align with reassigning to raster in a second
    if(class(v_mean) == "numeric"){
        v_mean <- data.frame(v_mean)
        v_sd <- data.frame(v_sd)
    }

    # Create a rasters matching CERRA
    r_mean <- rast(CERRA_ls[[1]][[1]], nlyrs = ncol(v_mean))
    r_sd <- rast(CERRA_ls[[1]][[1]], nlyrs = ncol(v_sd))

    # Assign values
    values(r_mean) <- v_mean
    values(r_sd) <- v_sd

    terra::time(r_mean) <- terra::time(r_sd) <- terra::time(x)

    # return object
    list(Mean = r_mean, SD = r_sd)
})
DEM_stack <- ContiStack_ls[[1]]
varnames(DEM_stack[[1]]) <- "Elevation Mean"
varnames(DEM_stack[[2]]) <- "Elevation SD"
units(DEM_stack[[1]]) <- units(DEM_stack[[2]]) <- "m"

AGB_stack <- ContiStack_ls[[2]]
varnames(AGB_stack[[1]]) <- "AGB Mean"
varnames(AGB_stack[[2]]) <- "AGB SD"
units(AGB_stack[[1]]) <- units(AGB_stack[[2]]) <- "t/ha"

##### Frequency of non-zero AGB values -----
print("Binarised AGB data")
BinAGB_ls <- pblapply(1:nlyr(ESA_agb_rast), FUN = function(lyr){
    # lyr <- 1
    lyrIter <- ESA_agb_rast[[lyr]]
    ## extract data
    raw_df <- extract(lyrIter > 0, polys)
    colnames(raw_df)[2] <- "value"
    ## summarise to 1s, 0s, pixel count, ratios
    ratio_df <- raw_df %>%
        group_by(ID) %>%
        summarise(
            ones  = sum(value == 1, na.rm = TRUE),
            zeros = sum(value == 0, na.rm = TRUE),
            All = length(value),
            ratioOneZero = ones / (ones + zeros),
            ratioOneAll = ones / All
        )
    head(ratio_df)
    ## save to raster
    r_ratio_NonNA <- rast(CERRA_ls[[1]][[1]])
    values(r_ratio_NonNA) <- ratio_df$ratioOneZero
    r_ratio_All <- rast(CERRA_ls[[1]][[1]])
    values(r_ratio_All) <- ratio_df$ratioOneAll
    names(r_ratio_NonNA) <- names(r_ratio_All) <- "AGB>0 Fraction"
    ## return data
    list(NonNA = r_ratio_NonNA, All = r_ratio_All)
})
AGBBinRatioNonNA_stack <- do.call(c, lapply(BinAGB_ls, "[[", 1))
AGBBinRatio_stack <- do.call(c, lapply(BinAGB_ls, "[[", 2))
terra::time(AGBBinRatioNonNA_stack) <- terra::time(AGBBinRatio_stack) <- terra::time(ESA_agb_rast)
varnames(AGBBinRatioNonNA_stack) <- "Non-Zero AGB Fraction ignoring NAs"
varnames(AGBBinRatio_stack) <- "Non-Zero AGB Fraction counting NAs as Zeroes"
units(AGBBinRatioNonNA_stack) <- units(AGBBinRatio_stack) <- "%"

##### CORINE Classifications as Frequencies -----
print("CORINE Land Cover Classifications")
vals <- extract(CORINE_rast, polys)   # no fun= argument!
colnames(vals) <- c("ID", "CORINE")

stop("which of these?!")
freq2 <- vals %>%
    group_by(ID, CORINE) %>%
    summarise(freq = n(), .groups = "drop")

freq2 <- vals %>%
    group_by(ID, CORINE) %>%
    summarise(freq = n(), .groups = "keep")

freq3 <- table(Data$ID, Data$CORINE)

freq_wide <- freq %>%
    tidyr::pivot_wider(
        names_from = test2,
        values_from = freq,
        values_fill = 0
    )
# convert data to matrix
mat <- as.matrix(freq_wide[,-1])
# make output raster with one layer per category
CORINE_stack <- rast(CERRA_ls[[1]][[1]], nlyrs = ncol(mat))
names(CORINE_stack) <- colnames(mat)
# fill raster stack
values(CORINE_stack) <- mat

names(CORINE_stack) <- colnames(mat)
varnames(CORINE_stack) <- colnames(mat)
units(CORINE_stack) <- ""

### Stacking -----
message("Saving Emulator-Ready NetCDFs")
print("AGB Rasters (4x)")
terra::writeCDF(AGB_stack[[1]], file = file.path(Dir.EmulatorData, "AGB_Mean.nc"), compression = 9)
terra::writeCDF(AGB_stack[[2]], file = file.path(Dir.EmulatorData, "AGB_SD.nc"), compression = 9)
terra::writeCDF(AGBBinRatioNonNA_stack, file = file.path(Dir.EmulatorData, "AGB_Ratio_ExNA.nc"), compression = 9)
terra::writeCDF(AGBBinRatio_stack, file = file.path(Dir.EmulatorData, "AGB_Ratio_InNA.nc"), compression = 9)

print("DEM Rasters (2x)")
terra::writeCDF(DEM_stack[[1]], file = file.path(Dir.EmulatorData, "DEM_Mean.nc"), compression = 9)
terra::writeCDF(DEM_stack[[2]], file = file.path(Dir.EmulatorData, "DEM_SD.nc"), compression = 9)

print("CORINE Raster (1x)")
terra::writeCDF(CORINE_stack, file = file.path(Dir.EmulatorData, "CORINE_Ratio.nc"), compression = 9)

### Combining into big data frame -----
stop("Continue here")
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
    Base_df$DAY <- substr(time(TAVG_Iter), 9, 10)
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

## Adding Derived Information ---------------------------------------------
Data_5km_df <- EmulatorReadying(Data_5km_df)
Data_5km_df <- na.omit(Data_5km_df)

write.csv(Data_5km_df, file.path(Dir.EmulatorData, "Data_5km_df.csv"))
saveRDS(Data_5km_df, file.path(Dir.EmulatorData, "Data_5km_df.rds"))

### Paralel Processing Preparations ---------------------------------------
## Readying for Moving Window
Data_5km_df$AGB_ESA <- unlist(Data_5km_df$AGB_ESA)
Data_5km_df$YEAR <- substr(Data_5km_df$YEAR_MONTH, 1, 4)
Data_5km_df$MONTH <- substr(Data_5km_df$YEAR_MONTH, 6, 7)
Data_5km_df <- na.omit(Data_5km_df)

## subset here for each location and its surrounding information
ULocs_df <- unique(Data_5km_df[, c("CELL", "LATITUDE", "LONGITUDE")])
lat_step <- min(diff(sort(unique(ULocs_df$LATITUDE)))) * 1.5
lon_step <- min(diff(sort(unique(ULocs_df$LONGITUDE)))) * 1.5

# Helper function to find neighbors
find_neighbors <- function(cell_row, all_cells, lat_step, lon_step) {
    lat <- cell_row$LATITUDE
    lon <- cell_row$LONGITUDE

    # Define bounding box for 3x3 neighborhood
    lat_range <- c(lat - lat_step, lat, lat + lat_step)
    lon_range <- c(lon - lon_step, lon, lon + lon_step)

    # Find all cells within that 3x3 box
    neighbors <- all_cells$CELL[all_cells$LATITUDE <= lat_range[3] & all_cells$LATITUDE >= lat_range[1] &
        all_cells$LONGITUDE <= lon_range[3] & all_cells$LONGITUDE >= lon_range[1]]

    # print(length(neighbors))
    return(neighbors)
}
# Apply function to each row
neighbor_list <- pbapply(ULocs_df, 1, function(row) {
    find_neighbors(as.list(row), ULocs_df, lat_step, lon_step)
})
# Attach the neighbors to our data
ULocs_df$neighbors <- neighbor_list

## figure out target cells (those that have non-zero AGB in ESA data)
non_zero_agb <- Data_5km_df$AGB_ESA != 0
non_zero_agb_count <- tapply(non_zero_agb, Data_5km_df$CELL, sum, na.rm = TRUE)
non_zero_agb_df <- data.frame(
    CELL = names(non_zero_agb_count),
    non_zero_agb_count = as.integer(non_zero_agb_count)
)

## split into individuals files for parallel processing
pbsapply(non_zero_agb_df$CELL[non_zero_agb_df$non_zero_agb_count != 0],
    cl = 5,
    FUN = function(LocIter) {
        print(LocIter)
        ## subset for location
        LocIter <- ULocs_df[ULocs_df$CELL == LocIter, ]
        FNAME <- file.path(Dir.EmulatorDataCERRA, paste0("CERRA_", LocIter$CELL, ".rds"))
        if (!file.exists(FNAME)) {
            ## subset data for location and its neighbours
            Loc_df <- Data_5km_df[which(Data_5km_df$CELL %in% unlist(LocIter$neighbors)), ]
            Loc_df <- Loc_df[, c(
                "CELL", "LONGITUDE", "LATITUDE", "YEAR_MONTH",
                "mean", "min", "max", "ELEVATION", "AGB_ESA"
            )]
            ## Saving data
            saveRDS(Loc_df, FNAME)
        }
    }
)
