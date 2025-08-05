#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - CERRA data
#'      + Download
#'      + Temporal Aggregation
#'      + Reprojection
#'  DEPENDENCIES:
#'  - CDS API Credentials in a file called PersonalSettings.R
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
# package_vec <- c(
#     "terra" # for spatraster operations
# )
# sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("ErikKusch/ClimHub")
}
library(ClimHub)
# package_vec <- c("ClimHub", package_vec)
source("PersonalSettings.R")

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.CERRA <- "/div/no-backup-nac/PATHFINDER/CERRA"
if (!dir.exists(Dir.CERRA)) {
    dir.create(Dir.CERRA)
}

# DATA ====================================================================
Meta_vec <- c(DOI = "10.24381/cds.622a565a", CITATION = "Schimanke S., Ridal M., Le Moigne P., Berggren L., Undén P., Randriamampianina R., Andrea U., Bazile E., Bertelsen A., Brousseau P., Dahlgren P., Edvinsson L., El Said A., Glinton M., Hopsch S., Isaksson L., Mladek R., Olsson E., Verrelle A., Wang Z.Q., (2021): CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.622a565a.", Project = "PATHFINDER", Handler = "Erik Kusch")

CERRA_ls <- lapply(2015:2023, FUN = function(YearIter) {
    message(YearIter)
    FileName <- paste0("CERRA_max_", YearIter, ".nc")

    if (file.exists(file.path(Dir.CERRA, FileName))) {
        print("Already prepared")
        list(
            mean = terra::rast(file.path(Dir.CERRA, paste0("CERRA_mean_", YearIter, ".nc"))),
            min = terra::rast(file.path(Dir.CERRA, paste0("CERRA_min_", YearIter, ".nc"))),
            max = terra::rast(file.path(Dir.CERRA, paste0("CERRA_max_", YearIter, ".nc")))
        )
    } else {
        CERRA_analysis_rast <- Download.reanalysis_cerra_single_levels(
            # CDS API call specification
            Variable = "2m temperature",
            LevelType = "Surface or atmosphere",
            SoilLayer = "",
            DataType = "Reanalysis",
            ProductType = "Analysis",
            LeadTimeHour = "",
            # time-window, default set to range of dataset-type
            DateStart = paste0(YearIter, "-01-01 01:00"),
            DateStop = paste0(YearIter, "-12-31 22:00"),
            TimeZone = "CET",
            # file naming and output
            Dir = Dir.CERRA,
            FileName = paste0("CERRA_", YearIter, ".nc"),
            Compression = NA,
            WriteFile = FALSE,
            RemoveTemporary = FALSE,
            # API credentials
            API_User = API_User,
            API_Key = API_Key,
            # Quality of life and efficiency settings
            TChunkSize = 6000,
            closeConnections = TRUE
        )

        CERRA_Year_ls <- list(
            CERRA_mean = Temporal.Aggregration(CERRA_analysis_rast, TResolution = "day", TStep = 1, FUN = mean),
            CERRA_min = Temporal.Aggregration(CERRA_analysis_rast, TResolution = "day", TStep = 1, FUN = min),
            CERRA_max = Temporal.Aggregration(CERRA_analysis_rast, TResolution = "day", TStep = 1, FUN = max)
        )

        CERRA_Year2_ls <- lapply(CERRA_Year_ls, FUN = function(x) {
            Repro <- Spatial.Reproject(x, ProjTo = 4326)
            Spatial.CropMask(Repro, terra::ext(c(-10, 30, 35, 70)))
        })

        CERRA_Year3_ls <- lapply(1:length(CERRA_Year2_ls), FUN = function(x) {
            terra::time(CERRA_Year2_ls[[x]]) <- as.POSIXct(terra::time(CERRA_Year2_ls[[x]]), "CET")
            ClimHub:::WriteRead.NC(
                NC = CERRA_Year2_ls[[x]],
                FName = file.path(Dir.CERRA, paste0(names(CERRA_Year2_ls)[[x]], "_", YearIter, ".nc")),
                Variable = "2m temperature",
                LongVar = paste(tools::file_path_sans_ext(names(CERRA_Year2_ls)[[x]]), "monthly air temperature"),
                Unit = "C",
                Attrs = Meta_vec, Write = TRUE, Compression = 9
            )
        })
        names(CERRA_Year3_ls) <- c("mean", "min", "max")

        unlink(list.files(Dir.CERRA, pattern = "TEMP_0000", full.names = TRUE))

        CERRA_Year3_ls
    }
})

CERRA_rasts_ls <- lapply(names(CERRA_ls[[1]]), FUN = function(x) {
    CERRA_iter <- do.call(c, lapply(CERRA_ls, "[[", x))
    ClimHub:::WriteRead.NC(
        NC = CERRA_iter,
        FName = file.path(Dir.CERRA, paste0("CERRA_", x, ".nc")),
        Variable = "2m temperature",
        LongVar = paste(x, "daily air temperature"),
        Unit = "C",
        Attrs = Meta_vec, Write = TRUE, Compression = 9
    )
    unlink(terra::sources(CERRA_iter))
})
