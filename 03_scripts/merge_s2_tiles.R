"
Earth Engine produces CSVs of band intensity for all s2 images in the area of interest
in the time of interest (see ee_sampling.js). This script merges all the CSVs into one file.
"

rm(list=ls())
library(tidyverse)
library(tools)

files <- list.files("01_data_in/ee_csvs/", pattern = '*.csv', full.names = T)
joined <- NA

for (path in files){
    band <- file_path_sans_ext(basename(path))
    tib <- read_csv(path) %>%
        select(measure_date = `system:time_start`, everything()) %>%
        mutate(measure_date = parse_date(measure_date, "%b %d, %Y")) %>%
        group_by(measure_date) %>%
        summarize_all(function(x) {
            max(c(x,-999), na.rm = T)
        }) %>%
        mutate_at(vars(WE4:WE13), ~ ifelse(. == -999, NA, .)) %>%
        gather("station", "band", -measure_date)
    
    if (is.na(joined)) {
        joined <- tib
    } else {
        joined <- left_join(joined, tib, by=c('measure_date'='measure_date', 'station'='station'))
    }
}

# I don't understand why gather won't accept a computed argument, so we have to do
# this garbage.
joined <- joined %>%
    select(
        measure_date = measure_date,
        station = station,
        B1  = band,
        B10 = band.x,
        B11 = band.y,
        B12 = band.x.x,
        B2  = band.y.y,
        B3  = band.x.x.x,
        B4  = band.y.y.y,
        B5  = band.x.x.x.x,
        B6  = band.y.y.y.y,
        B7  = band.x.x.x.x.x,
        B8  = band.y.y.y.y.y,
        B8A = band.x.x.x.x.x.x,
        B9  = band.y.y.y.y.y.y
    ) %>%
    drop_na() # Now get rid of all the masked observations

#write_csv(joined, '05_data_working/s2_observations.csv')