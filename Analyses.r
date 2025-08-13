#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Analyses / Forest Signal Emulator Building
#'  DEPENDENCIES:
#'  - All other scripts
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
    "geosphere" # for distance calculations in moving window
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
Dir.Base <- getwd()
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"

# DATA ====================================================================
message("####### Registering Data Files")
Fs <- list.files(Dir.EmulatorData, pattern = ".rds", full.names = TRUE)

# ANALYSES ================================================================
message("####### Carrying out Analyses")

## Linear Models ----------------------------------------------------------
message("+++ Linear Models +++")
lapply(Fs, FUN = function(FIter) {
    NameIter <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(basename(FIter)), pattern = "Data_", replacement = ""))
    FNAME <- file.path(Dir.EmulatorData, paste0("EmulatorResults_", NameIter, ".RData"))
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

        # Looping over outcomes that we want to model
        OutcomeIters <- c("mean", "range")
        Outcome_ls <- lapply(OutcomeIters, FUN = function(OutcomeIter) {
            # OutcomeIter = OutcomeIters[1]
            message(OutcomeIter)
            OutcomeModel_df <- ModelData_df
            colnames(OutcomeModel_df)[which(colnames(OutcomeModel_df) == OutcomeIter)] <- "Outcome"
            ## Basal models
            message("Base Estimates")
            Basemod <- lm(Outcome ~ AGB_ESA + LATITUDE + LONGITUDE + ELEVATION, data = OutcomeModel_df)
            Seasonmod <- lm(Outcome ~ AGB_ESA + AGB_ESA:SEASON + LATITUDE + LONGITUDE, data = OutcomeModel_df)

            ## Annual models
            message("Annual Estimates")
            YearIters <- unique(OutcomeModel_df$YEAR)
            YearIters <- YearIters[YearIters %in% as.character(2015:2022)]
            AnnualEstimates_ls <- lapply(YearIters, FUN = function(YearIter) {
                print(YearIter)
                ModelData_Iter <- OutcomeModel_df[OutcomeModel_df$YEAR == YearIter, ]
                list(
                    estimates = summary(lm(Outcome ~ AGB_ESA * SEASON + LATITUDE + LONGITUDE, data = ModelData_Iter))$coefficients,
                    RS2 = summary(lm(Outcome ~ AGB_ESA * SEASON + LATITUDE + LONGITUDE, data = ModelData_Iter))$r.squared
                )
            })
            names(AnnualEstimates_ls) <- YearIters

            ## Monthly Models
            message("Monthly Estimates")
            MonthIters <- str_pad(as.character(1:12), 2, "left", "0")
            MonthlyEstimates_ls <- lapply(MonthIters, FUN = function(MonthIter) {
                print(MonthIter)
                ModelData_Iter <- OutcomeModel_df[OutcomeModel_df$MONTH == MonthIter, ]
                list(
                    estimates = summary(lm(Outcome ~ AGB_ESA + LATITUDE + LONGITUDE, data = ModelData_Iter))$coefficients,
                    RS2 = summary(lm(Outcome ~ AGB_ESA + LATITUDE + LONGITUDE, data = ModelData_Iter))$r.squared
                )
            })
            names(MonthlyEstimates_ls) <- MonthIters

            # Return
            list(
                AllInOne = list(
                    Base = list(
                        estimates = summary(Basemod)$coefficients,
                        RS2 = summary(Basemod)$r.squared
                    ),
                    Seasons = list(
                        estimates = summary(Seasonmod)$coefficients,
                        RS2 = summary(Seasonmod)$r.squared
                    )
                ),
                Annual = AnnualEstimates_ls,
                Monthly = MonthlyEstimates_ls
            )
        })
        names(Outcome_ls) <- OutcomeIters
        save(Outcome_ls, file = FNAME)
    }
    Outcome_ls
})

## Location-Specific Models -----------------------------------------------
message("+++ Location-Specific Models +++")
LocSpec_ls <- lapply(Fs, FUN = function(FIter) {
    # FIter <- Fs[1]
    NameIter <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(basename(FIter)), pattern = "Data_", replacement = ""))
    FNAME <- file.path(Dir.EmulatorData, paste0("EmulatorResults_", NameIter, "_LocationSpecific.RData"))
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
        if ("VARIABLE" %in% colnames(ModelData_df)) {
            ModelData_df <- ModelData_df[ModelData_df$VARIABLE == "TAVG", ]
        }
        Locs_df <- ModelData_df[, c("LATITUDE", "LONGITUDE")]
        ULocs_df <- unique(Locs_df)

        Locs_ls <- pblapply(1:nrow(ULocs_df), cl = 50, FUN = function(LocIter) {
            # LocIter <- c(LATITUDE =  45.3667, LONGITUDE =  28.85)
            ## subset for location
            LocIter <- ULocs_df[LocIter, ]
            # print(LocIter)
            max_distance <- 25000 # 25 km
            ModelData_df$distance_m <- distHaversine( ## calculate distance in m
                p1 = Locs_df,
                p2 = LocIter
            )
            Loc_df <- subset(ModelData_df, distance_m <= max_distance) # subset for maximum distance

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
})
