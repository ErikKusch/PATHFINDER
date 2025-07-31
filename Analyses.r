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
    "ggplot2"
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
Dir.Base <- getwd()
Dir.EmulatorData <- "/div/no-backup-nac/PATHFINDER/EMULATOR-DATA"

# ANALYSES ================================================================
## Data -------------------------------------------------------------------
setwd(Dir.EmulatorData)
# RawData_df <- readRDS(list.files(pattern = ".rds")[2])
RawData_ls <- lapply(list.files(pattern = ".rds"), readRDS)
names(RawData_ls) <- gsub(pattern = "_df", replacement = "", gsub(tools::file_path_sans_ext(list.files(pattern = ".rds")), pattern = "Data_", replacement = ""))

## Linear Models ----------------------------------------------------------
Basemod <- lm(mean ~ AGB_ESA + LATITUDE + LONGITUDE + ELEVATION, data = ProcessedData_df)
summary(Basemod) # more forest = lower temperature

Seasonmod <- lm(mean ~ AGB_ESA + AGB_ESA : SEASON + LATITUDE + LONGITUDE, data = ProcessedData_df)
summary(Seasonmod) # more forest = cooling in Winter (stronger), Warming in summer (weaker) --> overall cooling

Rangemod <- lm(range ~ AGB_ESA + AGB_ESA : SEASON + LATITUDE + LONGITUDE, data = ProcessedData_df)
summary(Rangemod) # more forest = bigger range which gets even bigger in summer?!

# all of this is contradicotry to published papers!


# do the above analyses also year-by-year and also aggregated per month across years
stop("New models here")