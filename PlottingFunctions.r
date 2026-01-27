Panelplot <- function(data, marginals = "mean", marginalres = 0.1, col_neg = "#003575", col_pos = "#754000", Rescale_vec = NULL, legendlabel = "Coefficient Estimate") {
    symmetric_range <- function(x) {
        max_abs <- max(abs(range(x, na.rm = TRUE)))
        c(-max_abs, max_abs)
    }
    spacer <- ggplot() +
        theme_void()

    Estimate_gg <- ggplot(data, aes(x = LONGITUDE, y = LATITUDE, fill = estimate)) +
        geom_tile() +
        coord_fixed() +
        labs(
            fill = legendlabel
        ) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_text(hjust = 0.5))

    if (sum(sign(range(data$estimate, na.rm = TRUE))) == 0 | !is.null(Rescale_vec)) {
        MaxRange <- max(abs(c(max(data$estimate, na.rm = TRUE), min(data$estimate, na.rm = TRUE))))
        if(is.null(Rescale_vec)){
            Rescale_vec <- c(-MaxRange, -0.05, 0.05, MaxRange)
            Limits_vec <- symmetric_range(data$estimate)
        }else{
            Limits_vec <- c(Rescale_vec[1], tail(Rescale_vec, 1))
        }
        
        Estimate_gg <- Estimate_gg +
            scale_fill_gradientn(
                colors = c(col_neg, "grey", col_pos),
                values = scales::rescale(Rescale_vec),
                limits = Limits_vec,
                oob = scales::squish,
                na.value = "black" # NAs as black tiles
            )
    } else {
        Estimate_gg <- Estimate_gg +
            scale_fill_viridis_c(option = "plasma", na.value = "black") # NAs as black tiles
    }
    Estimate_gg <- Estimate_gg +
        guides(fill = guide_colourbar(title.position = "bottom", barwidth = unit(0.65, "npc")))

    if (marginals == "mean") {
        lon_df <- aggregate(estimate ~ LONGITUDE_bin, data = transform(data, LONGITUDE_bin = round(LONGITUDE / marginalres) * marginalres), FUN = mean, na.rm = TRUE)
        colnames(lon_df)[1] <- "LONGITUDE"
        lon_df$sign <- sign(lon_df$estimate)

        lat_df <- aggregate(estimate ~ LATITUDE_bin, data = transform(data, LATITUDE_bin = round(LATITUDE / marginalres) * marginalres), FUN = mean, na.rm = TRUE)
        colnames(lat_df)[1] <- "LATITUDE"
        lat_df$sign <- sign(lat_df$estimate)
        colnames(lon_df)[2] <- colnames(lat_df)[2] <- "value"
    }

    if (marginals == "fraction") {
        lon_agg <- aggregate(estimate ~ LONGITUDE_bin, data = transform(data, LONGITUDE_bin = round(LONGITUDE / marginalres) * marginalres), FUN = function(x) {
            pos <- sum(sign(x) == 1, na.rm = TRUE)
            neg <- sum(sign(x) == -1, na.rm = TRUE)
            total <- pos + neg
            if (total == 0) {
                return(c(value = 0, sign = 0))
            }
            frac_pos <- pos / total
            frac_neg <- neg / total
            value <- max(frac_pos, frac_neg)
            value <- value-0.5
            if(frac_neg > frac_pos){value <- -value}
            sign_dom <- if (frac_pos > frac_neg) 1 else -1
            if(frac_pos == 0.5){sign_dom = 0}
            c(value = value, sign = sign_dom)
        })
        lon_df <- data.frame(LONGITUDE = lon_agg$LONGITUDE_bin, value = lon_agg$estimate[, "value"], sign = lon_agg$estimate[, "sign"])

        lat_agg <- aggregate(estimate ~ LATITUDE_bin, data = transform(data, LATITUDE_bin = round(LATITUDE / marginalres) * marginalres), FUN = function(x) {
            pos <- sum(sign(x) == 1, na.rm = TRUE)
            neg <- sum(sign(x) == -1, na.rm = TRUE)
            total <- pos + neg
            if (total == 0) {
                return(c(value = 0, sign = 0))
            }
            frac_pos <- pos / total
            frac_neg <- neg / total
            value <- max(frac_pos, frac_neg)
            value <- value-0.5
            if(frac_neg > frac_pos){value <- -value}
            sign_dom <- if (frac_pos > frac_neg) 1 else -1
            if(frac_pos == 0.5){sign_dom = 0}
            c(value = value, sign = sign_dom)
        })
        lat_df <- data.frame(LATITUDE = lat_agg$LATITUDE_bin, value = lat_agg$estimate[, "value"], sign = lat_agg$estimate[, "sign"])
        
    }

    if (!isFALSE(marginals)) {
        lon_plot <- ggplot(lon_df, aes(x = LONGITUDE, y = value, fill = factor(sign))) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("-1" = col_neg, "1" = col_pos, "0" = "grey"), guide = "none") +
            theme_void() +
            theme(
                axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()
            )
        lat_plot <- ggplot(lat_df, aes(x = value, y = LATITUDE, fill = factor(sign))) +
            geom_bar(stat = "identity", orientation = "y") +
            scale_fill_manual(values = c("-1" = col_neg, "1" = col_pos, "0" = "grey"), guide = "none") +
            theme_void() +
            theme(
                axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()
            )
        Estimate_gg <- wrap_plots(A = lon_plot, B = spacer, C = Estimate_gg, D = lat_plot) +
            plot_layout(design = "AB\nCD", heights = c(0.1, 1), widths = c(1, 0.1))
    }
    Estimate_gg
}
