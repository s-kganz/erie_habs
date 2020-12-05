"
NOAA HAB monitoring data from Lake Erie is split by station and year across multiple files. This script
joins all the files together so we get one table of all stations and years.
"

rm(list=ls())
library(tidyverse)
library(tools)
library(janitor)
library(lubridate)
library(anytime)

stations <- c("WE02", "WE04", "WE08", "WE13")
data_dir <- "01_data_in\\erie_hab_monitoring\\buoys\\"

parse_buoy_csv <- function(path){
    col_names <- names(read_csv(path, n_max=0))
    con = file(path)
    units <- str_split(readLines(con, n=2)[2], pattern=',')[[1]]
    col_names <- paste(col_names, units, sep="_")
    
    # I can't find any documentaiton on what the flags mean so /shrug
    ret <- read_csv(path, col_names=col_names, skip=3, na=c("", "NA", "NaN")) %>%
        clean_names() %>%
        select(!contains('flags'))
    
    # Decompose the wind vector into x, y components
    ret <- ret %>%
        rename(wind_direction_heading = wind_direction_mean_degrees) %>%
        mutate(wind_direction_rads = ((450 - wind_direction_heading) %% 360) * 2 * pi / 360,
               wind_speed_north_meter_sec = wind_speed_mean_meters_per_second * sin(wind_direction_rads),
               wind_speed_east_meter_sec  = wind_speed_mean_meters_per_second * cos(wind_direction_rads))

    return(ret)
}

# Newer files look like this
new <- parse_buoy_csv("01_data_in\\erie_hab_monitoring\\buoys\\WE04_2016_annual_summary.csv")

for (station in stations) {
    files <- list.files(path=data_dir, pattern=paste0(station, '.*.csv'))
    full_df <- NA
    for (file in files){
        new_df <- parse_buoy_csv(file.path(data_dir, file))
        if (any('flags' %in% names(new_df))) { # discard this format of file
            print(paste0(file, " failed"))
        } else if (is.na(full_df)) { full_df <- new_df }
        else {
            full_df <- full_join(full_df, new_df)
        }
    }
    
    full_df <- full_df %>%
        mutate(timestamp_utc = mdy_hm(timestamp_utc),
               year = year(timestamp_utc),
               month = month(timestamp_utc),
               day = day(timestamp_utc)) %>%
        group_by(year, month, day) %>%
        summarize_all(mean) %>%
        mutate(station=station) %>%
        select(-c(timestamp_utc, wind_speed_max_meters_per_second), -contains("no3")) %>%
        select(station, everything())
    assign(station, full_df) # Use that station as the variable name
}

buoy_all <- bind_rows(WE02, WE04, WE08, WE13)
# Throw out columns where everything is NA
na_prop <- colMeans(is.na(buoy_all))
to_discard <- na_prop[na_prop==1]
buoy_all <- select(buoy_all, -names(to_discard))

na_prop_df <- data.frame(var = names(na_prop), val = na_prop, row.names=NULL)

# Finally, save everything out
out_dir <- "05_data_working"

write_csv(buoy_all, file.path(out_dir, "buoy_data_all.csv"))
write_csv(na_prop_df, file.path(out_dir, "buoy_data_na_prop.csv"))