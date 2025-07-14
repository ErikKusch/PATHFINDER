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
Dir.GHCN <- "/div/no-backup-nac/PATHFINDER/GHCN"

# DATA ====================================================================
## Files ------------------------------------------------------------------
Fs <- list.files(file.path(Dir.GHCN, "ghcnd"), pattern = ".csv", full.names = TRUE)

## Read data and reformat -------------------------------------------------
# Define the quality failure letters to check for according to https://www.ncei.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
bad_flags <- c("D", "G", "I", "K", "L", "M", "N", "O", "R", "S", "T", "W", "X", "Z")

Long_ls <- pblapply(Fs, cl = 36, 
FUN = function(FIter){
    # FIter <- Fs[1]
    # print(FIter)
    df <- read.csv(FIter)

    # Filter to years 2000–2024
    df <- df[as.numeric(substr(df$DATE, 1, 4)) >= 2000 & as.numeric(substr(df$DATE, 1, 4)) <= 2024, ]

    if(nrow(df) == 0){
        return(NA)
    }

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

    # remove non-data flags (all 9s)
    all_nines <- function(s) {
        digits <- unlist(regmatches(s, gregexpr("\\d", s)))
        length(digits) > 2 && all(digits == "9") # 9 and 99 may be real values
    }
    df$TMAX <- ifelse(sapply(df$TMAX, all_nines), NA, df$TMAX)
    df$TMIN <- ifelse(sapply(df$TMIN, all_nines), NA, df$TMIN)
    df$TAVG <- ifelse(sapply(df$TAVG, all_nines), NA, df$TAVG)

    # Invalidate TMAX/TMIN/TAVG if they contain any "bad" flags
    df$TMAX <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TMAX_ATTRIBUTES), NA, df$TMAX)
    df$TMIN <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TMIN_ATTRIBUTES), NA, df$TMIN)
    df$TAVG <- ifelse(grepl(paste(bad_flags, collapse = "|"), df$TAVG_ATTRIBUTES), NA, df$TAVG)

    # reformat to sensible degree C (data is stored in tens of degrees C)
    df$TMAX <- df$TMAX/10
    df$TMIN <- df$TMIN/10
    df$TAVG <- df$TAVG/10

    # Select and reshape
    Measurements <- df %>%
        select(STATION, DATE, LATITUDE, LONGITUDE, TMAX, TMIN, TAVG) %>%
        pivot_longer(
            cols = c(TMAX, TMIN, TAVG),
            names_to = "VARIABLE",
            values_to = "VALUE"
        ) 
        # %>%
        # filter(!is.na(VALUE))  # Remove rows where VALUE is NA
    
    # Elevation match
    ELEVATION <- df[1, c("STATION", "LATITUDE", "LONGITUDE", "ELEVATION")]

    # return
    list(
        Measurements = Measurements,
        Lookup = ELEVATION
    )
})

## Bind data, clean and export --------------------------------------------
Measurements_df <- do.call(rbind, lapply(Long_ls[!(unlist(lapply(Long_ls, class)) == "logical")], "[[", 1))
Measurements_df <- na.omit(Measurements_df)

Measurements_df$DATE <- as.POSIXct(Measurements_df$DATE)
Measurements_df$LATITUDE <- as.numeric(Measurements_df$LATITUDE)
Measurements_df$LONGITUDE <- as.numeric(Measurements_df$LONGITUDE)
Measurements_df$VALUE <- as.numeric(Measurements_df$VALUE)
Measurements_df$VARIABLE <- as.factor(Measurements_df$VARIABLE)

Measurements_df <- Measurements_df[Measurements_df$LONGITUDE >= -10 & Measurements_df$LONGITUDE <= 30 & Measurements_df$LATITUDE >= 35 & Measurements_df$LATITUDE <= 70, ] # limit to bounding box that all spatial data comes in

head(Measurements_df[Measurements_df$VARIABLE != "ELEVATION", ])

write.csv(Measurements_df, file = file.path(Dir.GHCN, "GHCN_Measurements_2000-2024_CLEANED.csv"))
saveRDS(Measurements_df, file = file.path(Dir.GHCN, "GHCN_Measurements_2000-2024_CLEANED.rds"))

Stations_df <- do.call(rbind, lapply(Long_ls[!(unlist(lapply(Long_ls, class)) == "logical")], "[[", 2))

write.csv(Stations_df, file = file.path(Dir.GHCN, "GHCN_Stations_2000-2024_CLEANED.csv"))
saveRDS(Stations_df, file = file.path(Dir.GHCN, "GHCN_Stations_2000-2024_CLEANED.rds"))

## Make monthly data summaries and export ---------------------------------
Stations_monthly_summary <- Measurements_df %>%
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
    # range = max - min,
    median = median(VALUE, na.rm = TRUE),
    .groups = "drop"
)
head(Stations_monthly_summary[Stations_monthly_summary$VARIABLE != "ELEVATION", ])

write.csv(Stations_monthly_summary, file.path(Dir.GHCN, "GHCN_2000-2024_MONTHLY.csv"))
saveRDS(Stations_monthly_summary, file.path(Dir.GHCN, "GHCN_2000-2024_MONTHLY.rds"))