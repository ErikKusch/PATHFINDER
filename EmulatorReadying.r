EmulatorReadying <- function(RawData_df){
    print("Adding derived information for emulator purposes")
# Seasons defined in-line with doi:10.1029/2023EF004220; MAM:March‐April‐May; JJA: June‐July‐August; SON: September‐October‐November; and DJF: December‐January‐February
    RawData_df$SEASON <- pbsapply(RawData_df$YEAR_MONTH, FUN = function(x){
        x <- substr(x, 6, 7)
        # print(x)
        if(x %in% c("03", "04", "05")){
            Ret <- "MAM"
        }
        if(x %in% c("06", "07", "08")){
            Ret <- "JJA"
        }
        if(x %in% c("09", "10", "11")){
            Ret <- "SON"
        }
        if(x %in% c("12", "01", "02")){
            Ret <- "DJF"
        }
        Ret
    })

    # Regions defined in-line with doi:10.1088/1748-9326/9/3/034002; might be better served with an overview of whether an area is predominantly boreal or deciduous forest; at present: everything is "North"
    RawData_df$REGION <- pbsapply(RawData_df$LATITUDE, FUN = function(x){
        ifelse(x > 35, "North", "South")
    })

    # Range between min and max to get a better understanding of temperature fluctuations
    RawData_df$range <- RawData_df$max - RawData_df$min

    # Returning data
    return(RawData_df)
    }