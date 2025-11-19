#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - CERRA data
#'      + Temporal Aggregation
#'      + Reprojection
#'  DEPENDENCIES:
#'  -
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
    "stringr", # for string operations
    "httr"
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("Clim-Hub/ClimHub")
}
library(ClimHub)
package_vec <- c("ClimHub", package_vec)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.CERRA <- file.path(Dir.Base, "CERRA")

# DATA ====================================================================
Meta_vec <- c(DOI = "10.24381/cds.622a565a", CITATION = "Schimanke S., Ridal M., Le Moigne P., Berggren L., Undén P., Randriamampianina R., Andrea U., Bazile E., Bertelsen A., Brousseau P., Dahlgren P., Edvinsson L., El Said A., Glinton M., Hopsch S., Isaksson L., Mladek R., Olsson E., Verrelle A., Wang Z.Q., (2021): CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.622a565a.", Project = "PATHFINDER", Handler = "Erik Kusch")

## DOWNLOAD ---------------------------------------------------------------
urls <- c(
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-2/2025-11-04/f3f041c13adaad09a8373c1f446e87e8.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-1/2025-11-04/249dff367c1100e985020e5304f9f529.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-2/2025-11-04/f9da5a364080c86a5f4bfce5dd61418b.grib",
    "https://object-store.os-api.cci2.ecmwf.int/cci2-prod-cache-3/2025-11-05/9f604b2d47ada2de44057b40d1161f79.grib"
)

# Function to extract filename from URL
get_filename <- function(url) {
    basename(url)
}

# Download each file if it doesn't exist
for (url in urls) {
    filename <- get_filename(url)
    filename <- file.path(getwd(), "CERRA", filename)

    if (!file.exists(filename)) {
        message("Downloading ", filename, "...")
        response <- GET(url, write_disk(filename, overwrite = TRUE))
        stop_for_status(response)
        message("Downloaded ", filename)
    } else {
        message("File ", filename, " already exists. Skipping download.")
    }
}

## PROCESSING -------------------------------------------------------------
Raw_FS <- list.files(Dir.CERRA, pattern = ".grib", full.names = TRUE)[-1]

CERRA_ls <- lapply(Raw_FS, FUN = function(FIter) {
    # FIter <- Raw_FS[1]
    CERRA_analysis_rast <- terra::rast(FIter)
    terra::longnames(CERRA_analysis_rast) <- unique(terra::names(CERRA_analysis_rast))

    var_name <- if (grepl("\\d+\\[m\\]", terra::longnames(CERRA_analysis_rast))) {
        # For height-specific variables
        height <- str_extract(terra::longnames(CERRA_analysis_rast), "\\d+(?=\\[m\\])")
        # Extract the actual variable name after semicolon and before final bracket
        var <- str_split(terra::longnames(CERRA_analysis_rast), ";")[[1]][2] %>%
            str_remove_all("\\[.*?\\]") %>% # remove [] and their contents
            str_trim() # remove trailing spaces
        paste0(height, "m ", var)
    } else {
        # For surface variables
        last_part <- str_split(terra::longnames(CERRA_analysis_rast), ";")[[1]][2] %>%
            str_remove_all("\\[.*?\\]") %>% # remove [] and their contents
            str_trim() # remove trailing spaces
        str_trim(last_part)
    }

    unit_name <- str_extract(terra::longnames(CERRA_analysis_rast), "\\[([^]]+)\\]$") %>%
        str_remove_all("\\[|\\]")

    message(var_name)
    print(unit_name)

    CERRA_analysis_rast <- CERRA_analysis_rast[[order(terra::time(CERRA_analysis_rast))]] # ensure time is ordered

    CERRA_Year_ls <- list(
        CERRA_mean = Temporal_Aggregation(CERRA_analysis_rast, tResolution = "day", tStep = 1, fun = mean),
        CERRA_min = Temporal_Aggregation(CERRA_analysis_rast, tResolution = "day", tStep = 1, fun = min),
        CERRA_max = Temporal_Aggregation(CERRA_analysis_rast, tResolution = "day", tStep = 1, fun = max)
    )

    CERRA_Year3_ls <- lapply(1:length(CERRA_Year_ls), FUN = function(x) {
        terra::time(CERRA_Year_ls[[x]]) <- as.POSIXct(terra::time(CERRA_Year_ls[[x]]), "CET")
        NC_Write(
            spatRaster = CERRA_Year_ls[[x]][[1]],
            fileName = file.path(Dir.CERRA, gsub(pattern = "CERRA", replacement = var_name, paste0(names(CERRA_Year_ls)[[x]], "NonRepro.nc"))),
            varName = var_name,
            longName = terra::longnames(CERRA_analysis_rast),
            unit = unit_name,
            meta = Meta_vec, compression = 9
        )
    })

    CERRA_Year2_ls <- lapply(CERRA_Year_ls, FUN = function(x) {
        Repro <- Spatial_Reproject(x, projTo = 4326)
        Spatial_Limit(Repro, terra::ext(c(-10, 30, 35, 70)))
    })
    names(CERRA_Year2_ls) <- c("mean", "min", "max")

    CERRA_Year4_ls <- lapply(1:length(CERRA_Year2_ls), FUN = function(x) {
        terra::time(CERRA_Year2_ls[[x]]) <- as.POSIXct(terra::time(CERRA_Year2_ls[[x]]), "CET")
        NC_Write(
            spatRaster = CERRA_Year2_ls[[x]],
            fileName = file.path(Dir.CERRA, gsub(pattern = "CERRA", replacement = var_name, paste0(names(CERRA_Year_ls)[[x]], ".nc"))),
            varName = var_name,
            longName = terra::longnames(CERRA_analysis_rast),
            unit = unit_name,
            meta = Meta_vec, compression = 9
        )
    })

    # unlink(list.files(Dir.CERRA, pattern = FIter, full.names = TRUE))

    CERRA_Year4_ls
})
