#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Analyses / Forest Signal Emulator Building
#'  DEPENDENCIES:
#'  - DATA_CERRA.r
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
    "pbapply",
    "stringr",
    "dplyr"
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
Dir.Base <- getwd()
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"

# DATA ====================================================================
message("####### Registering Data Files")
FIter <- list.files(Dir.EmulatorData, pattern = "Data_5km_df.rds", full.names = TRUE)

# ANALYSES ================================================================
message("####### Carrying out Analyses")

## Location-Specific Models -----------------------------------------------
NameIter <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(basename(FIter)), pattern = "Data_", replacement = ""))
FNAME <- file.path(Dir.EmulatorData, paste0("EmulatorResults_", NameIter, "_MovingWindow.RData"))
print(paste("--- Scale:", NameIter))
if (file.exists(FNAME)) {
    print("Already Compiled")
    load(FNAME)
} else {
        print("Compiling")
        ModelData_df <- readRDS(FIter) # ModelData_ls[[NameIter]]
        ## some more changes to the data frames
        ModelData_df$AGB_ESA <- unlist(ModelData_df$AGB_ESA)
        ModelData_df$YEAR <- substr(ModelData_df$YEAR_MONTH, 1, 4)
        ModelData_df$MONTH <- substr(ModelData_df$YEAR_MONTH, 6, 7)
        ModelData_df <- na.omit(ModelData_df)

        ## subset here for each location and its surrounding information
        ULocs_df <- unique(ModelData_df[, c("CELL", "LATITUDE", "LONGITUDE")])
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

#   neighbors <- all_cells %>%
#     dplyr::filter(LATITUDE %in% lat_range, LONGITUDE %in% lon_range)
#   print(length(neighbors))
  return(neighbors)
}

# Apply function to each row
neighbor_list <- pbapply(ULocs_df, 1, function(row) {
  find_neighbors(as.list(row), ULocs_df, lat_step, lon_step)
})

# Attach the neighbors to your data
ULocs_df$neighbors <- neighbor_list



        Locs_ls <- pblapply(1:nrow(ULocs_df), FUN = function(LocIter) {
            print(LocIter)
            # LocIter <- c(LATITUDE =  45.3667, LONGITUDE =  28.85)
            ## subset for location
            LocIter <- ULocs_df[LocIter, ]
            # print(LocIter)
            # if("CELL" %in% colnames(LocIter)){ # for gridded data, we do not use windows, but treat each cell as its own area
                Loc_df <- ModelData_df[which(ModelData_df$CELL %in%  unlist(LocIter$neighbors)), ]
            # }else{
            #     max_distance <- 25000 # 25 km
            #     ModelData_df$distance_m <- distHaversine( ## calculate distance in m
            #         p1 = Locs_df,
            #         p2 = LocIter
            #     )
            #     Loc_df <- subset(ModelData_df, distance_m <= max_distance) # subset for maximum distance
            # }
            # print("Subsetted")

            ## analysis
            # print(LocIter)
            if (length(unique(Loc_df$SEASON)) < 4 | all(tapply(Loc_df$AGB_ESA, Loc_df$SEASON, function(x) length(unique(x))) < 2)) {
                ## export of objects
                list(
                    Location = LocIter,
                    NumLocs = length(unique(Loc_df$STATION)),
                    NumMeasures = nrow(Loc_df),
                    Models = list(
                        Base = list(
                            estimates = NA,
                            RS2 = NA
                        ),
                        Seasons = list(
                            estimates = NA,
                            RS2 = NA
                        )
                    )
                )
            } else {
                Basemod <- lm(mean ~ AGB_ESA + ELEVATION, data = Loc_df)
                Seasonmod <- lm(mean ~ AGB_ESA + AGB_ESA:SEASON + ELEVATION, data = Loc_df) #  lm(mean ~ 0 + AGB_ESA:SEASON + ELEVATION, data = Loc_df)
                ## export of objects
                list(
                    Location = LocIter,
                    NumLocs = length(unique(Loc_df$STATION)),
                    NumMeasures = nrow(Loc_df),
                    Models = list(
                        Base = list(
                            estimates = summary(Basemod)$coefficients,
                            RS2 = summary(Basemod)$r.squared
                        ),
                        Seasons = list(
                            estimates = summary(Seasonmod)$coefficients,
                            RS2 = summary(Seasonmod)$r.squared
                        )
                    )
                )
            }
        })
        save(Locs_ls, file = FNAME)
    }
    Locs_ls