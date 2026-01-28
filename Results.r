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
    "stringr", # for string manipulation
    "patchwork", # for combining plots
    "tidybayes", # for stat_halfeyeh
    "rstatix", # for paired t test
    "tidyverse" # for merging of dataframes from list
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

# FUNCTIONALITY ===========================================================
source("PlottingFunctions.r")

label_row <- function(text) {
    ggplot() +
        geom_rect(
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "white",
            color = NA
        ) +
        annotate(
            "text",
            x = 0, y = 0,
            label = text,
            hjust = 0,
            fontface = "bold",
            size = 5
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), clip = "off") +
        theme_void() +
        theme(
            plot.margin = margin(0, 0, 0, 0)
        )
}

# PLOTS ===================================================================
Responses <- unique(Data_df$Response)
ModelTypes <- unique(Data_df$ModelType)
SeasonTypes <- unique(Data_df$Season)
plot_labs <- list(
    Summer = label_row("(A) Summer Seasons (JJA)"),
    Winter = label_row("(B) Winter Seasons (DJF)")
)

## Individual Models ======================================================
Res_ls <- lapply(Responses, function(Response) {
    # Response <- Responses[1]
    # message(Response)
    Dir.Response <- file.path(Dir.Exports, Response)
    if (!dir.exists(Dir.Response)) {
        dir.create(Dir.Response)
    }
    mods_ls <- lapply(ModelTypes, function(ModelType) {
        # ModelType <- ModelTypes[1]
        # print(ModelType)
        Dir.ModelType <- file.path(Dir.Response, ModelType)
        if (!dir.exists(Dir.ModelType)) {
            dir.create(Dir.ModelType)
        }
        plot_df <- Data_df[Data_df$Response == Response & Data_df$ModelType == ModelType, ]
        coefficients <- unique(plot_df$coefficient)

        ModFile <- file.path(Dir.ModelType, "R2.png")
        TFile <- file.path(Dir.ModelType, "SeasonComparison.png")

        message("Creating plots for ", Response, " using ", ModelType, " model.")
        ## plot generation
        coefs_ls <- lapply(coefficients, function(coefficient) {
            # coefficient <- "ForestFraction"
            print(coefficient)
            seasons_ls <- lapply(SeasonTypes, FUN = function(SeasonType) {
                # SeasonType <- "Winter"
                message(SeasonType)
                ggplot_df <- plot_df[plot_df$coefficient == coefficient & plot_df$Season == SeasonType, ]

                # set p-values that are NA to 1
                ggplot_df$p.value[is.na(ggplot_df$p.value)] <- 1

                # remove crazy extreme estimates
                ggplot_df[which(ggplot_df$estimate > quantile(ggplot_df$estimate, 0.99, na.rm = TRUE) |
                    ggplot_df$estimate < quantile(ggplot_df$estimate, 0.01, na.rm = TRUE)), c("estimate")] <- NA

                ## plot of estimates
                Estimate_gg <- Panelplot(
                    data = ggplot_df,
                    marginals = "mean",
                    marginalres = 0.1,
                    col_neg = "#003575",
                    col_pos = "#754000"
                )

                # plot of only significant estimates
                ggplot2_df <- ggplot_df
                ggplot2_df[which(ggplot2_df$p.value >= 0.05), c("estimate")] <- NA

                Estimate2_gg <- Panelplot(
                    data = ggplot2_df,
                    marginals = "mean",
                    marginalres = 0.1,
                    col_neg = "#003575",
                    col_pos = "#754000"
                )

                ## p-value plot
                pplot_df <- ggplot_df[, -which(colnames(ggplot_df) == "estimate")]
                colnames(pplot_df)[2] <- "estimate" # rename p-value to estimate column
                PVal_gg <- Panelplot(
                    data = pplot_df,
                    marginals = FALSE,
                    col_neg = "#067500",
                    col_pos = "#6F0075",
                    Rescale_vec = c(0, 0.04999, 0.050001, 1),
                    legendlabel = "P-Value"
                )

                ## R-squared
                Rplot_df <- ggplot_df[, -which(colnames(ggplot_df) == "estimate")]
                colnames(Rplot_df)[3] <- "estimate" # rename r-squared to estimate column
                Rplot_df$estimate[Rplot_df$estimate < 0] <- 0

                R2_2_gg <- Panelplot(
                    data = Rplot_df,
                    marginals = FALSE,
                    legendlabel = "R-Squared Model Fit"
                )

                # Return plots
                estimates_grid <- plot_grid(
                    plot_labs[[SeasonType]],
                    plot_grid(Estimate_gg, PVal_gg, Estimate2_gg, ncol = 1),
                    ncol = 1,
                    rel_heights = c(0.05, 1)
                )

                R2_2_gg <- plot_grid(
                    plot_labs[[SeasonType]],
                    R2_2_gg,
                    ncol = 1,
                    rel_heights = c(0.05, 1)
                )

                list(estimate = estimates_grid, R2 = R2_2_gg, data = ggplot2_df)
            })
            names(seasons_ls) <- SeasonTypes

            ## comparison across seasons with tidybayes
            summer_df <- na.omit(seasons_ls$Summer$data)
            winter_df <- na.omit(seasons_ls$Winter$data)
            winter_df <- winter_df[winter_df$p.value < 0.05, ]
            summer_df <- summer_df[summer_df$p.value < 0.05, ]
            analysis_df <- rbind(summer_df, winter_df)
            Cells <- intersect(winter_df$CELL, summer_df$CELL)
            t_test_df <- analysis_df[analysis_df$CELL %in% Cells, ]
            test_res <- t_test(estimate ~ Season, data = t_test_df, paired = TRUE)

            xlim_vals <- quantile(t_test_df$estimate, c(0.02, 0.98), na.rm = TRUE)

            TestComp_gg <- ggplot(t_test_df, aes(y = coefficient, x = estimate, fill = Season)) +
                stat_halfeye(alpha = 0.6, n = 3e3) +
                geom_vline(aes(xintercept = 0)) +
                theme_minimal() +
                scale_fill_manual(values = c("Winter" = "#003d46", "Summer" = "#ac4d20")) +
                coord_cartesian(xlim = xlim_vals) +
                labs(y = "", x = "Coefficient Estimate") +
                theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
                theme(legend.position = "bottom", legend.title = element_text(hjust = 0.5)) +
                annotate("text", x = xlim_vals[1] * 0.9, y = Inf, label = paste("Paired t-Test p-value =", format.pval(test_res$p, digits = 3)), hjust = 0, vjust = 5, size = 4)

            if (coefficient == tail(coefficients, 1) & !file.exists(ModFile)) {
                ggsave(
                    ModFile,
                    plot_grid(seasons_ls$Summer$R2, seasons_ls$Winter$R2, ncol = 2),
                    width = 16, height = 8.5
                )
            }

            coefficient <- gsub(pattern = ":", replacement = "_X_", coefficient)

            gg_file <- file.path(Dir.ModelType, paste0(coefficient, ".png"))
            if (!file.exists(gg_file)) {
                ggsave(
                    gg_file,
                    plot_grid(seasons_ls$Summer$estimate, seasons_ls$Winter$estimate, ncol = 2),
                    width = 6 * 2, height = 9.8 * 2
                )
            }

            # return only the data
            list(tTest = TestComp_gg, data = rbind(seasons_ls$Winter$data, seasons_ls$Summer$data))
        })
        names(coefs_ls) <- coefficients
        plots_ls <- lapply(coefs_ls, "[[", "tTest")

        spacers_ls <- lapply(names(plots_ls), label_row)

        combined_ls <- unlist(lapply(1:length(plots_ls), function(i) list(spacers_ls[[i]], plots_ls[[i]] + theme(legend.position = "none"))))

        legend_gg <- get_legend(plots_ls[[1]])
        SeasonComp_gg <- plot_grid(
            plot_grid(plotlist = combined_ls, ncol = 1, rel_heights = rep(c(0.2, 1), length(plots_ls))),
            legend_gg,
            ncol = 1,
            rel_heights = c(1, 0.1)
        )
        if (!file.exists(TFile)) {
            ggsave(TFile, SeasonComp_gg, width = 10, height = 3 * length(plots_ls))
        }

        data_ls <- lapply(coefs_ls, "[[", "data")
        data_ls
    })
    names(mods_ls) <- ModelTypes
    mods_ls
})
names(Res_ls) <- Responses


## Model Comparisons ======================================================
lapply(Responses, FUN = function(Response) {
    # Response <- Responses[2]
    Dir.Response <- file.path(Dir.Exports, Response)
    BestModF <- file.path(Dir.Response, "BestModel.png")
    BICDiffsF <- file.path(Dir.Response, "BICDifferences.png")

    BIC_ls <- lapply(ModelTypes, FUN = function(ModelType) {
        BIC_df <- Res_ls[[Response]][[ModelType]][["(Intercept)"]][, c("LONGITUDE", "LATITUDE", "Season", "BIC")]
        colnames(BIC_df)[4] <- ModelType
        BIC_df
    })
    BIC_df <- BIC_ls %>% reduce(left_join, by = c("LONGITUDE", "LATITUDE", "Season"))
    colnames(BIC_df)[-1:-3] <- c("~ ForestFraction", "~ ForestFraction + Roughness * Wind", "~ AGB + Roughness * Wind", "~ ForestFraction * AGB + Roughness * Wind")
    BestMod <- apply(BIC_df[, -1:-3], 1, function(x) {
        if (length(which.min(x)) == 0) {
            NA
        } else {
            which.min(x)
        }
    })
    BIC_df$BestMod <- colnames(BIC_df)[-1:-3][BestMod]
    # BIC_df$BestModNoNA <- BIC_df$BestMod
    # BIC_df$BestModNoNA[NARows] <- NA
    # head(BIC_df, 11)

    seasons_ls <- lapply(names(plot_labs), FUN = function(Season) {
        BIC_df <- BIC_df[BIC_df$Season == Season, ]

        ## BIC Differences
        BIC_df$Comb_Vs_Frac <- BIC_df[, "~ ForestFraction * AGB + Roughness * Wind"] - BIC_df[, "~ ForestFraction + Roughness * Wind"]
        BIC_df$Comb_Vs_Cont <- BIC_df[, "~ ForestFraction * AGB + Roughness * Wind"] - BIC_df[, "~ AGB + Roughness * Wind"]
        BIC_df$Cont_Vs_Frac <- BIC_df[, "~ AGB + Roughness * Wind"] - BIC_df[, "~ ForestFraction + Roughness * Wind"]
        Comps_ls <- lapply(c("Comb_Vs_Frac", "Comb_Vs_Cont", "Cont_Vs_Frac"), FUN = function(Comparison) {
            # print(Comparison)
            Iter_df <- BIC_df
            colnames(Iter_df)[which(colnames(Iter_df) == Comparison)] <- "estimate"
            Comb_Vs_Frac <- Panelplot(Iter_df)
        })

        ### Best Model as Map
        BestMod_gg <- ggplot(BIC_df, aes(x = LONGITUDE, y = LATITUDE, fill = BestMod)) +
            geom_tile() +
            scale_fill_viridis_d(na.value = "darkgrey") +
            coord_fixed() +
            labs(fill = "Model with Lowest BIC") +
            theme_bw() +
            theme(legend.position = "bottom", legend.title = element_text(hjust = 0.5))

        ### Pie chart
        df <- as.data.frame(table(BIC_df$BestMod, useNA = "ifany"))
        colnames(df) <- c("Model", "Count")
        df <- df %>%
            mutate(Percent = Count / sum(Count) * 100) %>%
            arrange(desc(Model)) %>%
            mutate(ypos = cumsum(Count) - 0.5 * Count)

        Pie_gg <- ggplot(df, aes(x = "", y = Count, fill = Model)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar(theta = "y") +
            scale_fill_viridis_d(na.value = "darkgrey") +
            theme_void() +
            geom_text(
                aes(
                    x = 2, y = ypos,
                    label = paste0(round(Percent, 1), "%")
                ),
                size = 3
            ) +
            labs(fill = "Model with Lowest BIC") +
            theme(legend.position = "none")

        ## Combining plots
        final_plot <- ggdraw(BestMod_gg + theme(legend.position = "none")) +
            draw_plot(Pie_gg, x = 0.2, y = 0.73, width = 0.25, height = 0.25)

        ## return objects for fusing
        list(
            Plot = plot_grid(
                plot_grid(plot_labs[[Season]]),
                final_plot,
                ncol = 1,
                rel_heights = c(0.05, 1)
            ),
            legend = get_legend(BestMod_gg),
            BICDiffs = plot_grid(
                label_row("I - Combined - Fraction Model"),
                Comps_ls[[1]],
                label_row("II - Combined - Continuous Model"),
                Comps_ls[[2]],
                label_row("III - Continuous - Fraction Model"),
                Comps_ls[[3]],
                ncol = 1,
                rel_heights = rep(c(0.05, 1), 3)
            )
        )
    })

    ## Best Model Plot
    Best_plot <- plot_grid(
        plot_grid(seasons_ls[[1]]$Plot, seasons_ls[[2]]$Plot, ncol = 2),
        seasons_ls[[1]]$legend,
        ncol = 1,
        rel_heights = c(1, 0.1)
    )
    ggsave(BestModF, Best_plot, width = 12, height = 6)

    ## BIC Difference Plot
    BICDiffs_plot <- plot_grid(
        plot_grid(
            plot_grid(plot_labs[[1]]),
            seasons_ls[[1]]$BICDiffs,
            ncol = 1,
            rel_heights = c(0.05, 1)
        ),
        plot_grid(
            plot_grid(plot_labs[[2]]),
            seasons_ls[[2]]$BICDiffs,
            ncol = 1,
            rel_heights = c(0.05, 1)
        ),
        ncol = 2
    )
    ggsave(BICDiffsF, BICDiffs_plot, width = 6 * 2, height = 9.8 * 2)
})
