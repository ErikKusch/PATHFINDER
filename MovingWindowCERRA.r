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
Dir.EmulatorDataCERRA <- file.path(Dir.EmulatorData, "CERRA")
Dir.EmulatorResults <- file.path(Dir.EmulatorData, "EmulatorResults")
if (!dir.exists(Dir.EmulatorResults)) dir.create(Dir.EmulatorResults)

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
        # unlink(list.files(Dir.EmulatorData, pattern = "CERRA_", full.names = TRUE))
        FS <- list.files(Dir.EmulatorDataCERRA, full.names = FALSE)
        # file.copy(from=file.path(Dir.EmulatorData, FS), to= file.path(Dir.EmulatorDataCERRA, FS), 
        #   overwrite = TRUE, recursive = FALSE, 
        #   copy.mode = TRUE)

        library(parallel)
        cl <- makeCluster(42L)
        clusterExport(cl, c("FS", "Dir.EmulatorDataCERRA", "Dir.EmulatorResults"))

        Results_ls <- pblapply(rev(FS), 
                            cl = cl, 
                            FUN = function(LocIter) {
            # message(LocIter)
            FRESULTS <- file.path(Dir.EmulatorResults, gsub(".rds", "_Results.rds", LocIter))
            if(file.exists(FRESULTS)){
                # print("compiled")
                Ret_ls <- readRDS(FRESULTS)
            }else{
                # print("compiling")
                Loc_df <- readRDS(file.path(Dir.EmulatorDataCERRA, LocIter))

            Loc_df$SEASON <- sapply(Loc_df$YEAR_MONTH, FUN = function(x){
                x <- substr(x, 6, 7)
                if(x %in% c("03", "04", "05")){
                    Ret <- "MAM"
                }
                if(x %in% c("06", "07", "08")){
                    Ret <- "JJA"
                }
                if(x %in% c("09", "10", "11")){
                    Ret <- "SON"
                }
                if(x %in% c("12", "01", "02")){
                    Ret <- "DJF"
                }
                Ret
            })

            numbers <- gregexpr("[0-9]+", LocIter)
            result <- regmatches(LocIter, numbers)
            Loc <- as.numeric(unlist(result))
            Coords <- Loc_df[which(head(Loc_df$CELL, 30) == Loc)[1], c("CELL", "LONGITUDE", "LATITUDE")]

                Basemod <- lm(mean ~ AGB_ESA + ELEVATION, data = Loc_df)
                Seasonmod <- lm(mean ~ AGB_ESA + AGB_ESA:SEASON + ELEVATION, data = Loc_df) #  lm(mean ~ 0 + AGB_ESA:SEASON + ELEVATION, data = Loc_df)
                ## export of objects
                Ret_ls <- list(
                    Location = Coords,
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
                saveRDS(Ret_ls, file = FRESULTS)
            }
            Ret_ls
        })

        stopCluster(cl)
        closeAllConnections()

        Results2_ls <- pblapply(Results_ls, FUN = function(x){ # this needs rework to fit with Coords export above
            R2 <- x$Models$Seasons$RS2
            Estimates <- x$Models$Seasons$estimates[,1]
            PVals <- x$Models$Seasons$estimates[,4]
            numbers <- gregexpr("[0-9]+", x$Location)
            result <- regmatches(x$Location, numbers)
            Loc <- as.numeric(unlist(result))

            Ret <- cbind(
                data.frame(
                    CELL = Loc, 
                    RS2 = R2
                    ),
                t(Estimates),
                t(PVals)
            )
            colnames(Ret) <- c("CELL", "R2", names(Estimates), paste0("p_", names(Estimates)))
            Ret[, -grep(colnames(Ret), pattern = "ELEVATION")]
        })
        Results_df <- do.call(rbind, Results2_ls)

        Locs_df <- do.call(rbind, pblapply(rev(FS), # this should be lifted into calculations above!!
                            # cl = cl, 
                            FUN = function(LocIter) {
                                Loc_df <- readRDS(file.path(Dir.EmulatorDataCERRA, LocIter))
                                numbers <- gregexpr("[0-9]+", LocIter)
                                result <- regmatches(LocIter, numbers)
                                Loc <- as.numeric(unlist(result))
                                Loc_df[which(head(Loc_df$CELL, 30) == Loc)[1], c("CELL", "LONGITUDE", "LATITUDE")]
                            }))

        ResultsMW_df <- merge(Results_df, Locs_df, by = "CELL")

        
        save( ResultsMW_df, file = FNAME)
    }
    # Locs_ls


250
