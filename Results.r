#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Visualisation of Forest Signal
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
    "ggplot2", # for plotting
    "ggpubr", # for publication ready plots
    "cowplot", # for arranging plots
    "stringr" # for string manipulation
)
sapply(package_vec, install.load.package)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.Exports <- file.path(Dir.Base, "Exports")
if (!dir.exists(Dir.Exports)) {
    dir.create(Dir.Exports)
}

# DATA ====================================================================
Data_df <- readRDS(file.path(Dir.Exports, "CERRA_LocationSpecificModelResults_df.rds"))
head(Data_df)

# PLOTS ===================================================================
Responses <- unique(Data_df$Response)
ModelTypes <- unique(Data_df$ModelType)

lapply(Responses, function(Response) {
    # Response <- Responses[1]
    # message(Response)
    Dir.Response <- file.path(Dir.Exports, Response)
    if (!dir.exists(Dir.Response)) {
        dir.create(Dir.Response)
    }
    lapply(ModelTypes, function(ModelType) {
        # ModelType <- ModelTypes[1]
        # print(ModelType)
        Dir.ModelType <- file.path(Dir.Response, ModelType)
        if (!dir.exists(Dir.ModelType)) {
            dir.create(Dir.ModelType)
        }
        plot_df <- Data_df[Data_df$Response == Response & Data_df$ModelType == ModelType, ]
        coefficients <- unique(plot_df$coefficient)

        ModFile <- file.path(Dir.ModelType, "R2.png")

        if (!ModFile %in% list.files(Dir.ModelType)) {
            message("Creating plots for ", Response, " using ", ModelType, " model.")
            lapply(coefficients, function(coefficient) {
                # coefficient <- "Surface_roughness_mean:Wind_speed_mean"
                print(coefficient)

                if (grepl(coefficient, pattern = ":") & coefficient != "Surface_roughness_mean:Wind_speed_mean") {
                    components <- str_split(coefficient, pattern = ":")[[1]]

                    ggplot1_df <- plot_df[plot_df$coefficient == components[[1]], ]
                    ggplot_df <- ggplot2_df <- plot_df[plot_df$coefficient == coefficient, ]

                    ggplot_df$estimate <- ggplot1_df$estimate + ggplot2_df$estimate

                    coefficient <- str_replace_all(coefficient, pattern = ":", replacement = "_x_")
                } else {
                    ggplot_df <- plot_df[plot_df$coefficient == coefficient, ]
                }

                # remove crazy extreme estimates
                ggplot_df <- ggplot_df[-which(ggplot_df$estimate > quantile(ggplot_df$estimate, 0.99, na.rm = TRUE) |
                    ggplot_df$estimate < quantile(ggplot_df$estimate, 0.01, na.rm = TRUE)), ]


                symmetric_range <- function(x) {
                    max_abs <- max(abs(range(x, na.rm = TRUE)))
                    c(-max_abs, max_abs)
                }

                ## estimate plot
                Estimate_gg <- ggplot(ggplot_df, aes(x = LONGITUDE, y = LATITUDE, fill = estimate)) +
                    geom_tile() +
                    coord_fixed() +
                    labs(
                        # title = paste("Estimate for coefficient", coefficient, "using", ModelType, "on Response:", Response),
                        fill = "Coefficient Estimate"
                    ) +
                    theme_minimal() +
                    theme(legend.position = "top", legend.title = element_text(hjust = 0.5))

                if (sum(sign(range(ggplot_df$estimate, na.rm = TRUE))) == 0) {
                    MaxRange <- max(abs(c(max(ggplot_df$estimate, na.rm = TRUE), min(ggplot_df$estimate, na.rm = TRUE))))
                    Estimate_gg <- Estimate_gg +
                        scale_fill_gradientn(
                            colors = c("#003575", "grey", "#754000"),
                            values = scales::rescale(c(-MaxRange, -0.05, 0.05, MaxRange)),
                            limits = symmetric_range(ggplot_df$estimate),
                            oob = scales::squish
                        )
                } else {
                    Estimate_gg <- Estimate_gg +
                        scale_fill_viridis_c(option = "plasma")
                }
                Estimate_gg <- Estimate_gg +
                    guides(fill = guide_colourbar(title.position = "top", barwidth = unit(0.65, "npc")))

                ## p-value plot
                PVal_gg <- ggplot(ggplot_df, aes(x = LONGITUDE, y = LATITUDE, fill = p.value)) +
                    geom_tile() +
                    coord_fixed() +
                    labs(
                        fill = "P-Value"
                    ) +
                    theme_minimal() +
                    theme(legend.position = "top", legend.title = element_text(hjust = 0.5))

                if (length(unique(ggplot_df$p.value < 0.05) == 2)) {
                    PVal_gg <- PVal_gg +
                        scale_fill_gradientn(
                            colors = c("#067500", "grey", "#6F0075"),
                            values = scales::rescale(c(0, 0.04999, 0.050001, 1))
                        )
                } else {
                    PVal_gg <- PVal_gg +
                        scale_fill_viridis_c()
                }
                PVal_gg <- PVal_gg +
                    guides(fill = guide_colourbar(title.position = "top", barwidth = unit(0.65, "npc")))

                panels_gg <- plot_grid(Estimate_gg, PVal_gg, ncol = 2)
                gg_file <- file.path(Dir.ModelType, paste0(coefficient, ".png"))
                ggsave(gg_file, panels_gg, width = 24, height = 18, units = "cm")
            })

            R2_2_gg <- ggplot(plot_df, aes(x = LONGITUDE, y = LATITUDE, fill = r.squared)) +
                geom_tile() +
                scale_fill_viridis_c() +
                coord_fixed() +
                labs(
                    # title = paste("R-squared for", Response, "using", ModelType, "model and coefficient:", coefficient),
                    fill = "R-squared"
                ) +
                theme_minimal() +
                theme(legend.position = "top", legend.title = element_text(hjust = 0.5)) +
                guides(fill = guide_colourbar(title.position = "top", barwidth = unit(0.65, "npc")))
            ggsave(ModFile, R2_2_gg, width = 16, height = 17, units = "cm")
        } else {
            message("Plots for ", Response, " using ", ModelType, " model already exist. Skipping...")
            return(NULL)
        }
    })
})
