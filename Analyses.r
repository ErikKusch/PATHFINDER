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
    "stringr"
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
Dir.Base <- getwd()
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"

# ANALYSES ================================================================
## Data -------------------------------------------------------------------
print("Data Loading")
# ModelData_df <- readRDS(file.path(Dir.EmulatorData, "Data_StationLevel.rds"))
Fs <- list.files(Dir.EmulatorData, pattern = ".rds", full.names = TRUE)
# ModelData_ls <- lapply(Fs, FUN = function(FIter) {
#     print(basename(FIter))
#     readRDS(FIter)
# })
# names(ModelData_ls) <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(basename(Fs)), pattern = "Data_", replacement = ""))

## Linear Models ----------------------------------------------------------
print("Analyses")
lapply(Fs, FUN = function(FIter) {
    NameIter <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(basename(FIter)), pattern = "Data_", replacement = ""))
    # NameIter = names(ModelData_ls)[2]
    FNAME <- file.path(Dir.EmulatorData, paste0("EmulatorResults_", NameIter, ".RData"))
    message(NameIter)
    if(file.exists(FNAME)){
        print("Already Compiled")
        load(FNAME)
    }else{
        print("Compiling")
        ModelData_df <- readRDS(FIter) # ModelData_ls[[NameIter]]
        message(NameIter)
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
