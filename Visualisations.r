#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Visualisation of Data and Results
#'  DEPENDENCIES:
#'  - Data files directly in working directory (to be executed on personal machine)
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
    "ggpubr", # for easy ggarrange
    "pbapply", # for progress bar
    "terra", # for spatraster operations
    "sf", # for polygon handling
    "ggplot2", # for plotting
    "rnaturalearth", # four outlines of countries
    "tidybayes", # nice distribution plotting
    "cowplot", # for grid plot constellations
    "dplyr" # for easier subsetting
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("ErikKusch/ClimHub")
}
library(ClimHub)
package_vec <- c("ClimHub", package_vec)

if ("KrigR" %in% rownames(installed.packages()) == FALSE) { # KrigR check
    devtools::install_github("ErikKusch/KrigR")
}
library(KrigR)
package_vec <- c("KrigR", package_vec)

# POLYGONS OF NATIONS =====================================================
Countries_sf <- ne_countries(scale = "medium")
Countries_sf <- st_crop(Countries_sf[, "admin"], xmin = -10, xmax = 30, ymin = 35, ymax = 70) # same as for the other data

# STATION-LEVEL DATA ======================================================
Stations_df <- readRDS("Data_StationLevel.rds")

## Station Locations ------------------------------------------------------
plot_df <- Stations_df[!duplicated(Stations_df$STATION), ]

Stations_gg <- ggplot() +
    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
    geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = STATION), col = "#ce2ad4", shape = 4) +
    theme_bw() +
    labs(title = paste0("GHCN Station Locations after Quality Assessment (n = ", nrow(plot_df), ")"))
ggsave(Stations_gg, file = "GHCN_StationLocations.png", width = 16, height = 16 * 1.2, unit = "cm")

## Station Number over the Years ------------------------------------------
plot_df <- as.data.frame(table(Stations_df$YEAR_MONTH))
colnames(plot_df) <- c("Date", "Stations")
plot_df <- plot_df[as.numeric(substr(plot_df$Date, 1, 4)) >= 2015 & as.numeric(substr(plot_df$Date, 1, 4)) <= 2022, ]
plot_df$Date <- as.POSIXct(plot_df$Date)

Stations_gg2 <- ggplot(plot_df, aes(x = Date, y = Stations)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(title = "GHCN Data Availability From 2015 to 2022 in Monthly Intervals after Quality Assessment")
ggsave(Stations_gg2, file = "GHCN_StationNumbers.png", width = 16 * 2, height = 16, unit = "cm")

# ## Temperature Data -------------------------------------------------------
# Stations_df$MONTH <- substr(Stations_df$YEAR_MONTH, 6, 7)
# plot_df <- aggregate(mean ~ LONGITUDE + LATITUDE + MONTH + VARIABLE, data = Stations_df, FUN = mean, na.rm = TRUE)
# plot_df$VARIABLE <- factor(plot_df$VARIABLE, levels = c("TMIN", "TAVG", "TMAX"))

# TemperatureStations_gg <- ggplot() +
#     geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
#     geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = mean)) +
#     facet_grid(MONTH ~ VARIABLE) +
#     # scale_colour_viridis_c() +  # does not work because colour is defined in points
#     scale_colour_gradient2(
#         low = "darkblue", # For negative values
#         mid = "lightgrey", # For zero
#         high = "darkred", # For positive values
#         midpoint = 0 # Ensures the color diverges around 0
#     ) +
#     theme_bw() +
#     labs(title = "GHCN Monthly Temperature Data", col = "")
# ggsave(TemperatureStations_gg, file = "GHCN_MonthlyTemperatures.png", width = 16 * 2, height = 16 * 10, unit = "cm", limitsize = FALSE)

## Forest Cover Data ------------------------------------------------------
ESA100m_rast <- rast("ESA-BIOMASS_100m_2015-2022.nc")[[1]]
ESA100_gg <- Plot.SpatRast(ESA100m_rast, SF = Countries_sf)
ggsave(ESA100_gg, file = "ESA100m.png", width = 16, height = 16, unit = "cm", limitsize = FALSE)

Stations_df$YEAR <- substr(Stations_df$YEAR_MONTH, 1, 4)
Stations_df$AGB_ESA <- unlist(Stations_df$AGB_ESA) # ToDo: need to fix this when preparing data!
plot_df <- aggregate(AGB_ESA ~ LONGITUDE + LATITUDE + YEAR, data = Stations_df, FUN = mean, na.rm = TRUE)

AGBStations_gg <- ggplot() +
    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
    geom_point(data = plot_df, aes(x = LONGITUDE, y = LATITUDE, col = log(AGB_ESA))) +
    facet_wrap(~YEAR, ncol = 4) +
    # scale_colour_viridis_c() +  # does not work because colour is defined in points
    scale_colour_gradient(
        low = "lightgrey",
        high = "darkgreen",
        na.value = "black"
    ) +
    theme_bw() +
    labs(title = "Logarithmic GHCN Annual ESA AGB Data (Black Points are NA locations)", col = "")
ggsave(AGBStations_gg, file = "GHCN_Annual_ESA-AGB.png", width = 16 * 4, height = 9 * 4, unit = "cm", limitsize = FALSE)

# 1km RESOLUTION DATA =====================================================

## CHELSA Data ------------------------------------------------------------
CHELSA_rast <- rast("CHELSA_tas2000-2019_MONTHLY.nc")[[1]]
CHELSA_gg <- Plot.SpatRast(CHELSA_rast, SF = Countries_sf)
ggsave(CHELSA_gg, file = "CHELSA.png", width = 16, height = 16, unit = "cm", limitsize = FALSE)

## Forest Cover Data ------------------------------------------------------
ESA1km_rast <- rast("ESA-BIOMASS_1km_2015-2022.nc")[[1]]
ESA1km_gg <- Plot.SpatRast(ESA1km_rast, SF = Countries_sf, COL = viridis::viridis(100), Legend = "AGB")
ggsave(ESA1km_gg, file = "ESA1km.png", width = 16, height = 16, unit = "cm", limitsize = FALSE)

## Elevation Data ---------------------------------------------------------
DEM_rast <- rast("GISCO_DEM.nc")[[1]]
DEM_gg <- Plot.SpatRast(DEM_rast, SF = Countries_sf, Legend = "DEM", COL = viridis::cividis(100))
ggsave(DEM_gg, file = "DEM.png", width = 16, height = 16, unit = "cm", limitsize = FALSE)

# 5km RESOLUTION DATA =====================================================

## CERRA Data -------------------------------------------------------------
CERRA_rast <- rast("CERRA_mean.nc")[[1]]
CERRA_gg <- Plot.SpatRast(CERRA_rast, SF = Countries_sf)
ggsave(CERRA_gg, file = "CERRA.png", width = 16, height = 16, unit = "cm", limitsize = FALSE)

# MODEL COEFFICIENTS ======================================================
Fs <- list.files(pattern = "EmulatorResults")

lapply(Fs, FUN = function(FIter) {
    # FIter <- Fs[3]
    print(FIter)
    load(FIter)
    ScaleName <- tools::file_path_sans_ext(gsub(pattern = "EmulatorResults_", replacement = "", FIter))
    if (grepl("_LocationSpecific", ScaleName)) {
        plot_df <- do.call(rbind, pblapply(1:length(Locs_ls), FUN = function(x) {
            # print(x)
            x <- Locs_ls[[x]]
            if (class(x$Models$Seasons$estimates)[1] == "logical") {
                Ret <- data.frame(Value = NA, Variable = NA)
            } else {
                Coeffs <- x$Models$Seasons$estimates
                Ret <- data.frame(
                    Value = c(
                        x$NumLocs,
                        x$NumMeasures,
                        Coeffs[which(rownames(Coeffs) == "AGB_ESA"), c(1, 4)],
                        # Coeffs[which(rownames(Coeffs) == "AGB_ESA:SEASONJJA"), c(1, 4)],
                        Coeffs[which(rownames(Coeffs) == "AGB_ESA"), 1] + Coeffs[which(rownames(Coeffs) == "AGB_ESA:SEASONJJA"), 1],
                        Coeffs[which(rownames(Coeffs) == "AGB_ESA:SEASONJJA"), 4],
                        x$Models$Seasons$RS2
                    ),
                    Variable = c("Number of Stations", "Number of Data Points", "Estimate (Winter)", "P-Value (Winter)", "Estimate (Summer)", "P-Value (Summer)", "R2")
                )
            }
            cbind(x$Location, Ret)
        }))
        plot_df <- na.omit(plot_df)
        ## removing outliers
        Quants <- quantile(plot_df$Value[grepl("Estimate", plot_df$Variable)], probs = c(0.05, 0.95))
        plot_df$Value[plot_df$Value < Quants[1] & grepl("Estimate", plot_df$Variable)] <- NA
        plot_df$Value[plot_df$Value > Quants[2] & grepl("Estimate", plot_df$Variable)] <- NA
        plot_df <- na.omit(plot_df)

        if (nrow(plot_df) / 7 > 1e5) { ## safe to assume we are dealing with closely clustered points
            GridFlag <- TRUE
        } else {
            GridFlag <- FALSE
        }

        # Pre-filter datasets
        stat_data <- filter(plot_df, Variable == "Number of Stations")
        data_data <- filter(plot_df, Variable == "Number of Data Points")
        r2_data <- filter(plot_df, Variable == "R2")

        # Shared base map
        base_map <- ggplot() +
            geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
            theme_bw()

        # Number of Stations Plot
        NumStat <- base_map +
            (if (GridFlag) {
                geom_tile(data = stat_data, aes(x = LONGITUDE, y = LATITUDE, fill = Value))
            } else {
                geom_point(data = stat_data, aes(x = LONGITUDE, y = LATITUDE, col = as.factor(Value)))
            }) +
            labs(title = "Number of Stations", col = "Value")

        # Number of Data Points Plot
        NumData <- base_map +
            (if (GridFlag) {
                geom_tile(data = data_data, aes(x = LONGITUDE, y = LATITUDE, fill = Value))
            } else {
                geom_point(data = data_data, aes(x = LONGITUDE, y = LATITUDE, col = log(Value)))
            }) +
            labs(title = "Number of Data Points", col = paste(ifelse(!GridFlag, "Log", ""), "Value"))

        # Model R Squared Plot
        (if (GridFlag) {
            RS2 <- base_map + geom_tile(data = r2_data, aes(x = LONGITUDE, y = LATITUDE, fill = Value)) +
                scale_fill_gradient2(low = "darkred", high = "darkgreen", midpoint = 0.5)
        } else {
            RS2 <- base_map + geom_point(data = r2_data, aes(x = LONGITUDE, y = LATITUDE, col = Value)) +
                scale_colour_gradient2(low = "darkred", high = "darkgreen", midpoint = 0.5)
        })
        RS2 <- RS2 + labs(title = "Model R squared")

        check <- c(
            list(
                plot_grid(NumStat, NumData, RS2, ncol = 3)
            ),
            lapply(c("(Winter)", "(Summer)"), FUN = function(Season) {
                pval_data <- filter(plot_df, Variable == paste("P-Value", Season))
                est_data <- filter(plot_df, Variable == paste("Estimate", Season))

                # Common map layer
                base_map <- ggplot() +
                    geom_sf(data = Countries_sf, colour = "black", fill = "#999177") +
                    theme_bw()

                # P-Values Plot
                (if (GridFlag) {
                    PValues <- base_map + geom_tile(data = pval_data, aes(x = LONGITUDE, y = LATITUDE, fill = Value < 0.05)) + scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"))
                } else {
                    PValues <- base_map + geom_point(data = pval_data, aes(x = LONGITUDE, y = LATITUDE, col = Value < 0.05)) + scale_colour_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"))
                })
                PValues <- PValues + labs(title = paste("P-values", Season))

                # Estimates Plot
                Estimates <- base_map +
                    labs(title = paste("Estimates", Season))

                # Add layers based on GridFlag
                if (GridFlag) {
                    Estimates <- Estimates +
                        geom_tile(data = est_data, aes(x = LONGITUDE, y = LATITUDE, fill = Value)) +
                        scale_fill_gradient2(
                            low = "darkblue", high = "darkgreen", midpoint = median(est_data$Value, na.rm = TRUE),
                            limits = quantile(est_data$Value, probs = c(0.05, 0.95)), oob = scales::squish
                        )
                } else {
                    Estimates <- Estimates +
                        geom_point(data = est_data, aes(x = LONGITUDE, y = LATITUDE, col = Value)) +
                        scale_colour_gradient2(
                            low = "darkblue", high = "darkgreen", midpoint = median(est_data$Value, na.rm = TRUE),
                            limits = quantile(est_data$Value, probs = c(0.05, 0.95)), oob = scales::squish
                        )
                }

                # Distribution Plot (not affected by GridFlag)
                Distrib <- ggplot(est_data, aes(x = Value)) +
                    stat_halfeye() +
                    theme_bw() +
                    geom_vline(xintercept = 0) +
                    labs(title = paste("Estimates", Season))

                plot_grid(Estimates, Distrib, PValues, ncol = 3)
            })
        )
        ggsave(plot_grid(plotlist = check, ncol = 1), file = paste0(ScaleName, ".png"), width = 16 * 3, height = 16 * 3, unit = "cm")
    } else {
        ## Annual Results
        AnnualPlot_df <- do.call(rbind, lapply(names(Outcome_ls), FUN = function(OutcomeIter) {
            AnnualPlot_df <- do.call(rbind, lapply(names(Outcome_ls[[OutcomeIter]]$Annual), FUN = function(YearIter) {
                # YearIter <- as.character(2015)

                Coeffs_df <- Outcome_ls[[OutcomeIter]]$Annual[[YearIter]]$estimates

                Estimate <- Coeffs_df[, "Estimate"]
                Estimates <- Estimate[which(names(Estimate) == "AGB_ESA")]
                Estimates <- c(Estimates, Estimates + Estimate[which(names(Estimate) == "AGB_ESA:SEASONJJA")])

                PValue <- Coeffs_df[, "Pr(>|t|)"]
                PValues <- c(PValue[which(names(Estimate) == "AGB_ESA")], PValue[which(names(Estimate) == "AGB_ESA:SEASONJJA")])

                data.frame(
                    R2 = c(Outcome_ls[[OutcomeIter]]$Annual[[YearIter]]$RS2, NA),
                    Pvalue = PValues,
                    Estimate = Estimates,
                    Season = c("Winter", "Summer"),
                    Year = YearIter,
                    Outcome = OutcomeIter
                )
            }))
            AnnualPlot_df
        }))

        Annual_gg <- ggplot(AnnualPlot_df, aes(x = Year, y = Estimate, shape = Pvalue < 0.05, col = Season, group = Season)) +
            geom_point() +
            geom_line() +
            geom_label(aes(label = round(R2, 2), y = -0.001), col = "black") + # Override y aesthetic here
            geom_hline(yintercept = 0, lty = 2) +
            facet_wrap(~Outcome, ncol = 1, scales = "free") +
            theme_bw()
        ggsave(Annual_gg, file = paste0(ScaleName, "_Annual.png"), width = 16 * 2, height = 16 * 1.2, unit = "cm")

        ## Monthly Results
        MonthlyPlot_df <- do.call(rbind, lapply(names(Outcome_ls), FUN = function(OutcomeIter) {
            MonthlyPlot_df <- do.call(rbind, lapply(names(Outcome_ls[[OutcomeIter]]$Monthly), FUN = function(MonthIter) {
                # MonthIter <- "01"

                Coeffs_df <- Outcome_ls[[OutcomeIter]]$Monthly[[MonthIter]]$estimates

                Estimate <- Coeffs_df[, "Estimate"]
                Estimates <- Estimate[which(names(Estimate) == "AGB_ESA")]

                PValue <- Coeffs_df[, "Pr(>|t|)"]
                PValues <- PValue[which(names(Estimate) == "AGB_ESA")]

                data.frame(
                    R2 = Outcome_ls[[OutcomeIter]]$Monthly[[MonthIter]]$RS2,
                    Pvalue = PValues,
                    Estimate = Estimates,
                    Month = MonthIter,
                    Outcome = OutcomeIter
                )
            }))
            MonthlyPlot_df
        }))

        Monthly_gg <- ggplot(MonthlyPlot_df, aes(x = Month, y = Estimate, shape = Pvalue < 0.05, group = 1)) +
            geom_point() +
            geom_line() +
            geom_label(aes(label = round(R2, 2), y = -0.001), col = "black") + # Override y aesthetic here
            geom_hline(yintercept = 0) +
            facet_wrap(~Outcome, ncol = 1, scales = "free") +
            theme_bw()
        ggsave(Monthly_gg, file = paste0(ScaleName, "_Monthly.png"), width = 16 * 2, height = 16 * 1.2, unit = "cm")
    }
})
