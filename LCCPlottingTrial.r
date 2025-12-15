rm(list=ls())

###############################################################
# Required packages
###############################################################
library(dplyr)
library(tidyr)
library(ggplot2)
# devtools::install_github("GuangchuangYu/ggtree")
library(ggtree)
# devtools::install_github("xiangpin/ggtreeExtra")
library(ggtreeExtra)
library(data.tree)
library(tidytree)
library(viridis)
library(ggpubr)
library(ggnewscale)
library(cowplot)
library(pbapply)

load(file = "LCC_Merged.rds")

head(Analysis_df)
dim(Analysis_df)
Analysis_df <- Analysis_df[!is.na(Analysis_df$LABEL1), ]

###############################################################
# STEP 0 — Boxplot per Grouping
###############################################################
Combins <- combn(unique(Analysis_df$LABEL2), 2, simplify = FALSE)

pvals <- pblapply(Combins, cl = 5, FUN = function(x){
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

bind <- lapply(pvals, FUN = function(x){
    data.frame(
        test = c("t", "chi"),
        estimate = c(abs(diff(x[["t"]]$estimate)), x[["chi"]]$statistic),
        pvals = c(x[["t"]]$p.value, x[["chi"]]$p.value)
    )
})
bind <- do.call(rbind, bind)
Stats_df <- cbind(Stats_df, bind)
Stats_df <- Stats_df[Stats_df$LCC1 != Stats_df$LCC2, ]

###############################################################
# STEP 1 — Summarise data into percentages per LABEL3 × AGB_Bin
###############################################################
tile_df <- Analysis_df %>%
    group_by(LABEL1, LABEL2, AGB_Bin) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(LABEL1, LABEL2) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()
tile_df
tile_df <- tile_df[tile_df$AGB_Bin != 0, ]

###############################################################
# STEP 2 — Build CORINE hierarchy as a data.tree structure
###############################################################
# Need only the unique hierarchical structure
hier_df <- tile_df %>%
    distinct(LABEL1, LABEL2) %>%
    mutate(pathString = paste("CORINE", LABEL1, LABEL2, sep = "+"))

# Convert to a tree
corine_tree <- FromDataFrameTable(
    table = hier_df, pathDelimiter = "+"
)
corine_tree

###############################################################
# STEP 3 — Convert to phylo object for ggtree
###############################################################
tree <- as.phylo(corine_tree)

###############################################################
# STEP 4 — Ensure LABEL2 ordering matches heatmap rows
###############################################################
tip_order <- tree$tip.label

sanitize <- function(x) {
    gsub("\\s+", "_", x)
}
tile_df$tip_label <- sanitize(gsub(tile_df$LABEL2, pattern = ",", replacement = ""))
tree$tip.label <- sanitize(tree$tip.label)
tile_df$tip_label <- factor(tile_df$tip_label) #, levels = tree$tip.label)

Stats_df$LCC1 <- sanitize(gsub(Stats_df$LCC1, pattern = ",", replacement = ""))
Stats_df$LCC2 <- sanitize(gsub(Stats_df$LCC2, pattern = ",", replacement = ""))
Analysis_df$LABEL2 <- sanitize(gsub(Analysis_df$LABEL2, pattern = ",", replacement = ""))

###############################################################
# STEP 6 — Create the dendrogram + heatmap
###############################################################
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
        color = "black",   # border color
        size = 0.3,        # border thickness
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
            label = paste(round(pct*100, 2), "%")  # show % as 0–100
        ),
        offset = 0.02,
        pwidth = 0.6,
        size = 3       # text size
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
        title.position = "top",   # put the title above the bar
        title.hjust = 0.5,       # center the title
        barwidth = unit(15, "cm"),  # width of the color bar
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

p2 <- ggplot(Analysis_df, aes(y = LABEL2, x = AGB)) + 
    # geom_violin() + 
    geom_boxplot() + 
    # geom_boxplot(width = 0.2) + 
    labs(x = "Above Ground Biomass", y = "") + 
    theme_bw() + 
    theme(axis.text.y=element_blank())

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
Stats_df2$minLCC = with(Stats_df2, pmin(LCC1, LCC2))
Stats_df2$maxLCC = with(Stats_df2, pmax(LCC1, LCC2))

p3 <- ggplot() +
    ## t test data
    geom_tile(data = Stats_df2[Stats_df2$test == "t" , ], aes(fill = estimate, x = maxLCC, y = minLCC)) +
    geom_label(data = Stats_df2[Stats_df2$test == "t" , ], aes(label = round(pvals, 4), , x = maxLCC, y = minLCC)) +
    scale_fill_viridis_c(option = "E", na.value="transparent", name = "t-test: Mean AGB Group Difference", position = "bottom") +
    ## chi squared
    new_scale_fill() +
    geom_tile(data = Stats_df2[Stats_df2$test == "chi" , ], aes(fill = estimate, x = minLCC, y = maxLCC)) +
    geom_label(data = Stats_df2[Stats_df2$test == "chi" , ], aes(label = round(pvals, 4), , x = minLCC, y = maxLCC)) +
    scale_fill_viridis_c(option = "B", na.value="transparent", name = "Chi-Squared: Test Statistic", position = "top") +
    ## styling
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "")

###############################################################
# STEP 6 — Print the final plot
###############################################################
p <- cowplot::plot_grid(
        cowplot::plot_grid(
            p1, p2, nrow = 1
        ),
        p3, ncol = 1
    )
ggsave(p, file = "LCCAlignment2.png", width = 16*2, height = 12*2)
