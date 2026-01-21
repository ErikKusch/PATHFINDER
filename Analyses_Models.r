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
    "pbapply", # for progress bars with estimator
    "mgcv"
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
Dir.ExportsCERRA <- file.path(Dir.Exports, "LocDFs")
if (!dir.exists(Dir.ExportsCERRA)) {
    dir.create(Dir.ExportsCERRA)
}

# DATA ====================================================================
message("####### Registering Data Files")
Fs <- list.files(Dir.EmulatorDataCERRA, pattern = ".rds", full.names = TRUE)

# ANALYSES ================================================================
message("####### Carrying out Analyses")
## response variables
Responses <- c(
                "Temperature_mean", "SkinVsTemperature_diff" #  "Temperature_max", "Temperature_min", "Temperature_range", "Skin_temperature_mean", "Skin_temperature_max", "Skin_temperature_min", "Skin_temperature_range",
            )

## Actual Analyses ---------------------------------------------------------
message("+++ Location-Specific Models +++")

model_summary <- function(model) {
    if (is.null(model)) return(NULL)

    list(
        coefficients = summary(model)$p.table,
        # smooths = summary(model)$s.table,
        AIC = AIC(model),
        BIC = BIC(model),
        r_squared =  summary(model)$r.sq
    )
}

get_coef_df <- function(mod_obj) {
                if (is.null(mod_obj)) {
                    return(NULL)
                }

                coefs <- as.data.frame(mod_obj$coefficients, stringsAsFactors = FALSE)
                df <- data.frame(
                    coefficient = rownames(coefs),
                    estimate = coefs$Estimate,
                    p.value = coefs[["Pr(>|t|)"]],
                    row.names = NULL,
                    stringsAsFactors = FALSE
                )
                # Add R-squared as a column
                df$r.squared <- mod_obj$r_squared
                df$AIC <- mod_obj$AIC
                df$BIC <- mod_obj$BIC
                df
            }

LocSpec_ls <- pblapply(
    Fs,
    cl = 5,
    FUN = function(FIter) {
        # FIter <- "/storage/no-backup-nac/PATHFINDER/Manuscript/EmulatorData/CERRA/CERRA_10113.rds" # Fs[1]
        NameIter <- gsub(pattern = "_df", replacement = "", tools::file_path_sans_ext(basename(FIter)))
        FNAME <- file.path(Dir.ExportsCERRA, paste0(NameIter, ".rds"))
        # RDSNAME <- file.path(Dir.Exports, gsub(pattern = "RData", replacement = "rds", basename(FNAME)))
        CELL <- gsub(pattern = ".rds", replacement = "", gsub(pattern = "CERRA_", replacement = "", basename(FNAME)))
        # print(NameIter)
        if (file.exists(FNAME)) {
            # print("Already Compiled")
            df <- readRDS(FNAME)
        } else {
            # print(NameIter)
            # loading data -----------------
            basedata <- ModelData_df <- readRDS(FIter) # ModelData_ls[[NameIter]]
            ModelData_df$YEAR <- substr(ModelData_df$YEAR_MONTH, 1, 4)
            ModelData_df$MONTH <- substr(ModelData_df$YEAR_MONTH, 6, 7)
            colnames(ModelData_df) <- sub("^[0-9]+m_", "", colnames(ModelData_df)) # remove leading numbers
            ModelData_df$SkinVsTemperature_diff <- ModelData_df[, "Skin_temperature_mean"] - ModelData_df[, "Temperature_mean"]

            ## models -----------------
            ## actual models
            models_ls <- lapply(Responses, FUN = function(ResponseVar) {
                # ResponseVar <- Responses[1]
                # print(ResponseVar)
                Seasons <- c("Winter" = "DJF", "Summer" = "JJA")
                seasons_ls <- lapply(Seasons, FUN = function(Season){
                    # Season <- Seasons[1]
                    # extracting relevant data for season
                    iter_df <- ModelData_df[ModelData_df$SEASON == Season, ]
                    # Create a date column (assuming YEAR/MONTH/DAY are in standard formats)
                    iter_df$date <- as.Date(paste(iter_df$YEAR, iter_df$MONTH, iter_df$DAY, sep = "-"))
                    # Sort data by CELL and date (essential for time-series correlation)
                    iter_df <- iter_df[order(iter_df$CELL, iter_df$date), ]
                    colnames(iter_df)[which(colnames(iter_df) == "AGBRatioNonNA")] <- "ForestFraction"

                    iter_df_clean <- na.omit(iter_df[, c(ResponseVar, "AGB_Mean", "ForestFraction", "Surface_roughness_mean", "Wind_speed_mean", "date", "CELL")])

                    ### AGB Forest Fraction ------
                    FracMod_AGB_ForestOnly <- bam(
                        as.formula(paste0(ResponseVar, " ~ ForestFraction")),
                        data = iter_df_clean,
                        correlation = corExp(form = ~ date | CELL) #corAR1(form = ~ date | CELL)  # AR(1) per spatial group
                    )

                    FracMod_AGB_Full <- bam(
                        as.formula(paste0(ResponseVar, " ~ ForestFraction + Surface_roughness_mean * Wind_speed_mean")),
                        data = iter_df_clean,
                        correlation = corExp(form = ~ date | CELL) #corAR1(form = ~ date | CELL)  # AR(1) per spatial group
                    )

                    ### Continuous AGB ------
                    ContMod <- bam(
                        as.formula(paste0(ResponseVar, " ~AGB_Mean + Surface_roughness_mean * Wind_speed_mean")),
                        data = iter_df_clean,
                        correlation = corExp(form = ~ date | CELL) 
                        )

                    ### Combined Model ------
                    CombMod <- bam(
                        as.formula(paste0(ResponseVar, " ~ AGB_Mean * ForestFraction + Surface_roughness_mean * Wind_speed_mean")),
                        data = iter_df_clean,
                        correlation = corExp(form = ~ date | CELL)
                        )

                    ## building list -----------------
                    models_ls <- list(
                        FracMod_ForestOnly = model_summary(FracMod_AGB_ForestOnly),
                        FracMod_Full = model_summary(FracMod_AGB_Full),
                        ContMod = model_summary(ContMod),
                        CombMod = model_summary(CombMod)
                    )
                    models_ls
                })
                names(seasons_ls) <- names(Seasons)
                seasons_ls
            })
            names(models_ls) <- Responses

            df <- lapply(Responses, FUN = function(Response) {
                # print(Response)
                # Response <- Responses[1]
                models_df <- lapply(names(models_ls[[Response]]), FUN = function(season_name) {
                    # season_name <- "Winter"
                    seasons_df <- lapply(names(models_ls[[Response]][[season_name]]), FUN = function(model_name){
                        # model_name <- "ContMod"
                        df <- get_coef_df(models_ls[[Response]][[season_name]][[model_name]])
                        df$ModelType <- model_name
                        df
                    })
                    seasons_df <- do.call(rbind, seasons_df)
                    seasons_df$Season <- season_name
                    seasons_df
                })
                df <- do.call(rbind, models_df[which(unlist(lapply(models_df, class)) == "data.frame")])
                df$Response <- Response
                df
            })
            df <- do.call(rbind, df)
            df$CELL <- CELL
            df$LONGITUDE <- basedata[which(basedata$CELL == CELL)[1], "LONGITUDE"]
            df$LATITUDE <- basedata[which(basedata$CELL == CELL)[1], "LATITUDE"]
            saveRDS(df, file = FNAME)
        }
        return(df)
    }
)
ModelResults_df <- do.call(rbind, LocSpec_ls)
dim(ModelResults_df)
head(ModelResults_df)
saveRDS(ModelResults_df, file = file.path(Dir.Exports, "CERRA_LocationSpecificModelResults_df.rds"))
# RMFiles <- list.files(Dir.Exports, pattern = ".rds", full.names = TRUE)
# RMFiles <- RMFiles[-grep(RMFiles, pattern = "CERRA_LocationSpecificModelResults_df.rds")]