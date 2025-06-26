#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Load and compile GHCN station data
#'  DEPENDENCIES:
#'  - GHCN station data CSV files in respective PATHFINDER directory on no-backup-nac
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

package_vec <- c(
    "tidyr", # piping
    "dplyr", # pivoting
    "pbapply", # parallelisation and progress
    "lubridate" # for date operations
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.GHCN <- "/div/no-backup-nac/PATHFINDER/GHCN/ghcnd"

# DATA ====================================================================
## Files ------------------------------------------------------------------
Fs <- list.files(Dir.GHCN, pattern = ".csv", full.names = TRUE)

## Read data and reformat -------------------------------------------------
# Define the quality failure letters to check for according to https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
bad_flags <- c("D", "G", "I", "K", "L", "M", "N", "O", "R", "S", "T", "W", "X", "Z")

Long_ls <- pblapply(Fs, cl = 12, FUN = function(FIter){
    # print(FIter)
    df <- read.csv(FIter)

    expected_vars <- c("ELEVATION", "TMAX", "TMIN", "TAVG") # "PRCP", "SNWD", 
    attr_vars <- c("TMAX_ATTRIBUTES", "TMIN_ATTRIBUTES", "TAVG_ATTRIBUTES")

    # Add missing expected vars
    for (var in expected_vars) {
        if (!var %in% names(df)) {
            df[[var]] <- NA
        }
    }

    # Add missing attribute vars
    for (attr in attr_vars) {
        if (!attr %in% names(df)) {
            df[[attr]] <- ""
        }
    }

    # Filter to years 2000–2024
    df <- df[as.numeric(substr(df$DATE, 1, 4)) >= 2000 & as.numeric(substr(df$DATE, 1, 4)) <= 2024, ]

    # Invalidate TMAX/TMIN/TAVG if they contain any "bad" flags
    df$TMAX <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TMAX_ATTRIBUTES), NA, df$TMAX)
    df$TMIN <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TMIN_ATTRIBUTES), NA, df$TMIN)
    df$TAVG <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TAVG_ATTRIBUTES), NA, df$TAVG)

    # Select and reshape
    df %>%
        select(STATION, DATE, LATITUDE, LONGITUDE, ELEVATION, TMAX, TMIN, TAVG) %>%
        pivot_longer(
            cols = c(ELEVATION, TMAX, TMIN, TAVG),
            names_to = "VARIABLE",
            values_to = "VALUE"
        ) %>%
        filter(!is.na(VALUE))  # Remove rows where VALUE is NA
})

## Bind data, clean and export --------------------------------------------
Stations_df <- do.call(rbind, Long_ls)
Stations_df <- Stations_df[Stations_df$LONGITUDE >= -10 & Stations_df$LONGITUDE <= 30 & Stations_df$LATITUDE >= 35 & Stations_df$LATITUDE <= 70, ] # limit to bounding box that all spatial data comes in
write.csv(Stations_df, file = "GHCN_2000-2024.csv")

## Make monthly data summaries and export ---------------------------------
Stations_monthly_summary <- Stations_df %>%
mutate(
    DATE = ymd(DATE),                      # Convert to Date class
    YEAR_MONTH = floor_date(DATE, "month")  # Get month (1st day of each month)
) %>%
group_by(STATION, VARIABLE, YEAR_MONTH) %>%
summarise(
    mean = mean(VALUE, na.rm = TRUE),
    sd = sd(VALUE, na.rm = TRUE),
    min = min(VALUE, na.rm = TRUE),
    max = max(VALUE, na.rm = TRUE),
    range = max - min,
    median = median(VALUE, na.rm = TRUE),
    .groups = "drop"
)
head(Stations_monthly_summary)

write.csv(Stations_monthly_summary, "GHCN_2000-2024_MONTHLY.csv")