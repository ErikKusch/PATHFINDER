#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Analyses of Forest Signal
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
    "stringr", # for string padding
    "pbapply" # for progress bars with estimator
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.EmulatorData <- file.path(Dir.Base, "EmulatorData")
Dir.EmulatorDataCERRA <- file.path(Dir.EmulatorData, "CERRA")
Dir.Exports <- file.path(Dir.Base, "Exports")
if (!dir.exists(Dir.Exports)) {
    dir.create(Dir.Exports)
}
Dir.ExportsCERRA <- file.path(Dir.Exports, "CERRA")
if (!dir.exists(Dir.ExportsCERRA)) {
    dir.create(Dir.ExportsCERRA)
}

# DATA ====================================================================
message("####### Registering Data Files")
Fs <- list.files(Dir.EmulatorDataCERRA, pattern = ".rds", full.names = TRUE)

# ANALYSES ================================================================
message("####### Carrying out Analyses")

message("+++ Location-Specific Models +++")
LocSpec_ls <- pblapply(
    Fs,
    # cl = 30,
    FUN = function(FIter) {
        # FIter <- "/storage/no-backup-nac/PATHFINDER/Manuscript/EmulatorData/CERRA/CERRA_10113.rds" # Fs[1]
        NameIter <- gsub(pattern = "_df", replacement = "", tools::file_path_sans_ext(basename(FIter)))
        FNAME <- file.path(Dir.ExportsCERRA, paste0(NameIter, ".RData"))
        print(NameIter)
        if (file.exists(FNAME)) {
            # print("Already Compiled")
            load(FNAME)
        } else {
            # print("Compiling")
            ## loading data -----------------
            ModelData_df <- readRDS(FIter) # ModelData_ls[[NameIter]]
            ModelData_df$YEAR <- substr(ModelData_df$YEAR_MONTH, 1, 4)
            ModelData_df$MONTH <- substr(ModelData_df$YEAR_MONTH, 6, 7)
            colnames(ModelData_df) <- sub("^[0-9]+m_", "", colnames(ModelData_df)) # remove leading numbers
            ModelData_df$`Temperature_range` <- ModelData_df[, "Temperature_max"] - ModelData_df[, "Temperature_min"]
            ModelData_df$Skin_temperature_range <- ModelData_df[, "Skin_temperature_max"] - ModelData_df[, "Skin_temperature_min"]
            ModelData_df$SkinVsTemperature_diff <- ModelData_df[, "Skin_temperature_mean"] - ModelData_df[, "Temperature_mean"]
            ModelData_df$AGB_Binary <- ifelse(ModelData_df$AGB_Mean > 0, "Forest", "Non-Forest")
            ModelData_df$CORINE_FF <- ModelData_df$CORINE_23 + ModelData_df$CORINE_24 + ModelData_df$CORINE_25

            ## models -----------------
            ## response variables
            Responses <- c(
                "Temperature_mean", "Temperature_max", "Temperature_min", "Temperature_range",
                "Skin_temperature_mean", "Skin_temperature_max", "Skin_temperature_min", "Skin_temperature_range",
                "SkinVsTemperature_diff"
            )

            ## actual models
            models_ls <- lapply(Responses, FUN = function(ResponseVar) {
                # ResponseVar <- Responses[1]
                # print(ResponseVar)
                ### Binarised AGB ------
                if (length(na.omit(unique(ModelData_df$AGB_Binary))) != 2) {
                    BinMod <- NULL
                } else {
                    BinMod <- lm(as.formula(paste0(ResponseVar, " ~ AGB_Binary*SEASON + Surface_roughness_mean*Wind_speed_mean")), data = ModelData_df)
                }

                ### Continuous AGB ------
                ContMod <- lm(as.formula(paste0(ResponseVar, " ~ AGB_Mean*SEASON + Surface_roughness_mean*Wind_speed_mean")), data = ModelData_df)

                ### AGB Forest Fraction ------
                FracMod_AGB_NonNA <- lm(as.formula(paste0(ResponseVar, " ~ AGBRatioNonNA*SEASON + Surface_roughness_mean*Wind_speed_mean")), data = ModelData_df)
                FracMod_AGB <- lm(as.formula(paste0(ResponseVar, " ~ AGBBinRatio*SEASON + Surface_roughness_mean*Wind_speed_mean")), data = ModelData_df)

                ### Corine Forest Fraction ------
                FracMod_CORINE <- lm(as.formula(paste0(ResponseVar, " ~ CORINE_FF*SEASON + Surface_roughness_mean*Wind_speed_mean")), data = ModelData_df)

                ## building list -----------------
                model_summary <- function(model) {
                    if (is.null(model)) {
                        return(NULL)
                    }
                    list(
                        coefficients = coef(model),
                        summary = summary(model)
                    )
                }
                models_ls <- list(
                    BinMod = model_summary(BinMod),
                    ContMod = model_summary(ContMod),
                    FracMod_AGB_NonNA = model_summary(FracMod_AGB_NonNA),
                    FracMod_AGB = model_summary(FracMod_AGB),
                    FracMod_CORINE = model_summary(FracMod_CORINE)
                )
                models_ls
            })
            names(models_ls) <- Responses

            ## saving results -----------------
            save(models_ls, file = FNAME)
        }
        # return(models_ls)
    }
)
