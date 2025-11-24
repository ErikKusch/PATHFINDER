#' ####################################################################### #
#' PROJECT: [PATHFINDER - WP3]
#' CONTENTS:
#'  - Overlap of ESA CCI Biomass an CORINE Land Cover
#'      + Load data
#'      + transform ESA CCI Biomass to binary representation of forest/non-forest
#'      + Calculate overlap with CORINE Land Cover forest classes
#'  DEPENDENCIES:
#'  - data in CORINE subdirectory (produced through manual download)
#'  - data in ESACCI-BIOMASS subdirectory (produced by DATA_ESACCI-Biomass.r)
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
    "dplyr",
    "tidyr",
    "ggplot2",
    "data.tree",
    "tidytree",
    "viridis",
    "ggpubr",
    "ggnewscale",
    "cowplot",
    "pbapply"
)
sapply(package_vec, install.load.package)

### NON-CRAN PACKAGES ----
if ("ClimHub" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("ErikKusch/ClimHub")
}
library(ClimHub)
package_vec <- c("ClimHub", package_vec)

if ("ggtreeExtra" %in% rownames(installed.packages()) == FALSE) { # ClimHub check
    devtools::install_github("GuangchuangYu/ggtree")
    devtools::install_github("xiangpin/ggtreeExtra")
}
library(ggtree)
library(ggtreeExtra)
package_vec <- c("ggtree", "ggtreeExtra", package_vec)

## Directories ------------------------------------------------------------
### Define directories in relation to project directory
Dir.Base <- getwd() # identifying the current directory
Dir.ESA <- file.path(Dir.Base, "ESACCI-BIOMASS")
Dir.CORINE <- file.path(Dir.Base, "CORINE")
Dir.EmulatorData <- file.path(Dir.Base, "EmulatorData")
if (!dir.exists(Dir.EmulatorData)) {
    dir.create(Dir.EmulatorData)
}

# DATA IMPORT AND PREPARATION =============================================
## Load ESA CCI Biomass data ----------------------------------------------
ESA_rast <- terra::rast(file.path(Dir.ESA, "ESA-BIOMASS_100m_2015-2022.nc"))[[seq(from = 1, to = 12, by = 2)]] # don't need to keep the SD layers
# ESA_rast <- ESA_rast[[grep(terra::time(ESA_rast), pattern = "2020")[1]]] # no need to keep sd layer

## Load CORINE Land Cover data --------------------------------------------
CORINE_rast <- terra::rast(file.path(Dir.CORINE, "u2018_clc2018_v2020_20u1_raster100m", "DATA", "U2018_CLC2018_V2020_20u1.tif"))

## Reproject ESA CCI to CORINE Land Cover ---------------------------------
ESARepro_rast <- lapply(1:nlyr(ESA_rast), FUN = function(x) {
    print(terra::time(ESA_rast[[x]]))
    Spatial_Reproject(ESA_rast[[x]], CORINE_rast, rasterResample = TRUE)
})
ESARepro_rast <- do.call(c, ESARepro_rast)

## Cropping & Masking Corine to reprojected ESA CCI extent ----------------
CORINE_crop <- terra::crop(CORINE_rast, terra::ext(ESARepro_rast))
CORINE_crop[is.na(ESARepro_rast)] <- NA # align NAs
ESARepro_rast[is.na(CORINE_crop)] <- NA # align NAs

EsaSafe <- Spatial_Reproject(ESARepro_rast, 4326)
CORINESafe <- Spatial_Reproject(CORINE_crop, 4326)
terra::writeCDF(EsaSafe, file = file.path(Dir.EmulatorData, "AGB_Aligned.nc"), compression = 9)
terra::writeCDF(CORINESafe, file = file.path(Dir.EmulatorData, "CORINE_Aligned.nc"), compression = 9)

ESARepro_rast <- ESARepro_rast[[1]]
stop("rempve this stop")

## Binarising AGB data ----------------------------------------------------
AGB_Bin <- ESARepro_rast > 0

## Making Data Products into Data Frame Object ----------------------------
DF <- terra::as.data.frame(CORINE_crop, xy = TRUE)
colnames(DF) <- c("x", "y", "CORINE")
DF$AGB <- values(ESARepro_rast)
DF$AGB_Bin <- values(AGB_Bin)
# saveRDS(DF, file = "LCC_AGB_100m.rds")

## Translating CORINE Classes< --------------------------------------------
CORINE_df <- read.csv2(file.path(Dir.CORINE, "clc_legend.csv"), header = TRUE, stringsAsFactors = FALSE)
Analysis_df <- merge(DF, CORINE_df, by.x = "CORINE", by.y = "GRID_CODE", all.x = TRUE)
# save(Analysis_df, file = "LCC_Merged.rds")

LABEL1_tab <- table(Analysis_df$LABEL1, Analysis_df$AGB_Bin)
LABEL2_tab <- table(Analysis_df$LABEL2, Analysis_df$AGB_Bin)
LABEL3_tab <- table(Analysis_df$LABEL3, Analysis_df$AGB_Bin)
LCC_Corine_ls <- list(Label1 = LABEL1_tab, Label2 = LABEL2_tab, Label3 = LABEL3_tab)

# LAND COVER CLASSIFICATION (LCC) ALIGNMENT WITH AGB DATA =================
Analysis_df <- Analysis_df[!is.na(Analysis_df$LABEL1), ] # remove "no-CorineClass" pixels

## Statistical Tests of CORINE Label2 classes and AGB values --------------
Combins <- combn(unique(Analysis_df$LABEL2), 2, simplify = FALSE)

pvals <- pblapply(Combins, cl = 5, FUN = function(x) {
    ## continuous
    data1 <- Analysis_df$AGB[Analysis_df$LABEL2 == x[1]]
    data2 <- Analysis_df$AGB[Analysis_df$LABEL2 == x[2]]
    data <- c(data1, data2)
    labels <- c(rep(x[1], length(data1)), rep(x[2], length(data2)))
    t_test <- t.test(x = data1, y = data2)
    ## binary
    data <- rbind(
        table(Analysis_df$AGB_Bin[Analysis_df$LABEL2 == x[1]]),
        table(Analysis_df$AGB_Bin[Analysis_df$LABEL2 == x[2]])
    )
    rownames(data) <- x
    chitest <- chisq.test(data)

    list(t = t_test, chi = chitest)
})

Stats_df <- data.frame(
    LCC1 = rep(unlist(lapply(Combins, "[[", 1)), each = 2),
    LCC2 = rep(unlist(lapply(Combins, "[[", 2)), each = 2)
)

bind <- lapply(pvals, FUN = function(x) {
    data.frame(
        test = c("t", "chi"),
        estimate = c(abs(diff(x[["t"]]$estimate)), x[["chi"]]$statistic),
        pvals = c(x[["t"]]$p.value, x[["chi"]]$p.value)
    )
})
bind <- do.call(rbind, bind)
Stats_df <- cbind(Stats_df, bind)
Stats_df <- Stats_df[Stats_df$LCC1 != Stats_df$LCC2, ]

## Percentage tiles of non-zero binary AGB data in CORINE Label2 clases ---
tile_df <- Analysis_df %>%
    group_by(LABEL1, LABEL2, AGB_Bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(LABEL1, LABEL2) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
tile_df
tile_df <- tile_df[tile_df$AGB_Bin != 0, ]

## CORINE hierarchy as a data.tree/ggtree structure -----------------------
# Need only the unique hierarchical structure
hier_df <- tile_df %>%
    distinct(LABEL1, LABEL2) %>%
    mutate(pathString = paste("CORINE", LABEL1, LABEL2, sep = "+"))

# Convert to a tree
corine_tree <- FromDataFrameTable(
    table = hier_df, pathDelimiter = "+"
)

# convert to ggtree
tree <- as.phylo(corine_tree)

## Santising and harmonising labels for plotting --------------------------
tip_order <- tree$tip.label

sanitize <- function(x) {
    gsub("\\s+", "_", x)
}
tile_df$tip_label <- sanitize(gsub(tile_df$LABEL2, pattern = ",", replacement = ""))
tree$tip.label <- sanitize(tree$tip.label)
tile_df$tip_label <- factor(tile_df$tip_label) # , levels = tree$tip.label)

Stats_df$LCC1 <- sanitize(gsub(Stats_df$LCC1, pattern = ",", replacement = ""))
Stats_df$LCC2 <- sanitize(gsub(Stats_df$LCC2, pattern = ",", replacement = ""))
Analysis_df$LABEL2 <- sanitize(gsub(Analysis_df$LABEL2, pattern = ",", replacement = ""))

## Plot creation ----------------------------------------------------------
### Dendrogram plot with percentage tiles ------
p1 <- ggtree(tree, layout = "rectangular", branch.length = "none") +
    # Add tip labels
    # geom_tiplab(aes(label = label)) +
    geom_label(aes(label = label), hjust = 1, colour = "black") +

    # Attach tile heatmap to the right side
    geom_fruit(
        data = tile_df,
        geom = geom_tile,
        mapping = aes(
            y = tip_label,
            x = AGB_Bin,
            fill = pct
        ),
        color = "black", # border color
        size = 0.3, # border thickness
        width = 0.4,
        offset = -0.495, # spacing to tips of dendrogram
        pwidth = 0.6
    ) +

    geom_fruit(
        data = tile_df,
        geom = geom_label,
        mapping = aes(
            y = tip_label,
            x = factor(AGB_Bin),
            label = paste(round(pct * 100, 2), "%") # show % as 0–100
        ),
        offset = 0.02,
        pwidth = 0.6,
        size = 3 # text size
    ) +

    labs(
        x = ""
        # ,
        # title = "CORINE Class Hierarchy with Binary ESA AGB Percentages"
    ) +

    scale_fill_viridis(
        name = "Percentage non-zero AGB cells [%]",
        option = "D",
        end = 0.75,
        guide = guide_colorbar(
            title.position = "top", # put the title above the bar
            title.hjust = 0.5, # center the title
            barwidth = unit(15, "cm"), # width of the color bar
            barheight = unit(0.5, "cm") # thickness
        )
    ) +

    # styling
    theme(
        legend.position = "bottom",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9)
    )

### Boxplot of Continuous AGB in CORINE classes ------
p2 <- ggplot(Analysis_df, aes(y = factor(LABEL2, levels = tip_order), x = AGB)) +
    # geom_violin() +
    geom_boxplot() +
    # geom_boxplot(width = 0.2) +
    labs(x = "Above Ground Biomass", y = "") +
    theme_bw()
# +
# theme(axis.text.y = element_blank())

Stats_df2 <- Stats_df
Stats_df2 <- rbind(
    Stats_df2,
    data.frame(
        LCC1 = unique(Stats_df2$LCC1),
        LCC2 = unique(Stats_df2$LCC1),
        test = "t",
        estimate = NA,
        pvals = NA
    )
)
Stats_df2$LCC1 <- factor(Stats_df2$LCC1, levels = tip_order, ordered = TRUE)
Stats_df2$LCC2 <- factor(Stats_df2$LCC2, levels = tip_order, ordered = TRUE)
## use pmin and pmax to get the row-wise min and max factor values
Stats_df2$minLCC <- with(Stats_df2, pmin(LCC1, LCC2))
Stats_df2$maxLCC <- with(Stats_df2, pmax(LCC1, LCC2))

### Tile plot of statistical differentiation between AGB values in CORINE classes ------
p3 <- ggplot() +
    ## t test data
    geom_tile(data = Stats_df2[Stats_df2$test == "t", ], aes(fill = estimate, x = maxLCC, y = minLCC)) +
    geom_label(data = Stats_df2[Stats_df2$test == "t", ], aes(label = round(pvals, 4), , x = maxLCC, y = minLCC)) +
    scale_fill_viridis_c(option = "E", na.value = "transparent", name = "t-test: Mean AGB Group Difference", position = "bottom") +
    ## chi squared
    new_scale_fill() +
    geom_tile(data = Stats_df2[Stats_df2$test == "chi", ], aes(fill = estimate, x = minLCC, y = maxLCC)) +
    geom_label(data = Stats_df2[Stats_df2$test == "chi", ], aes(label = round(pvals, 4), , x = minLCC, y = maxLCC)) +
    scale_fill_viridis_c(option = "B", na.value = "transparent", name = "Chi-Squared: Test Statistic", position = "top") +
    ## styling
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "")

## Saving the final plot --------------------------------------------------
p <- cowplot::plot_grid(
    cowplot::plot_grid(
        p1, p2,
        nrow = 1
    ),
    p3,
    ncol = 1
)
ggsave(p, file = "LCCAlignment.png", width = 16 * 2, height = 12 * 2)
