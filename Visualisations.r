library(ggplot2)

Measurements_df <- readRDS("GHCN_2000-2024_MONTHLY.rds")
Measurements_df$month <- format(as.POSIXct(Measurements_df$YEAR_MONTH), "%m")


Aggregate_df <- Measurements_df[Measurements_df$VARIABLE == "TAVG", ]
dim(Aggregate_df)

head(Aggregate_df)
Aggregated_df <- aggregate(mean ~ STATION * month, FUN = mean, data = Aggregate_df)


Stations_df <- readRDS("GHCN_Stations_2000-2024_CLEANED.rds")

plot_df <- cbind(Aggregated_df, Stations_df[match(Aggregated_df$STATION, Stations_df$STATION), c("LONGITUDE", "LATITUDE")])

ggplot(plot_df, aes(x = LONGITUDE, y = LATITUDE, col = mean)) +
    geom_point() +
    facet_wrap(~month) +
    scale_color_viridis_c() +
    theme_bw()
