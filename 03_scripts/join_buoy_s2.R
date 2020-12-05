# Buoy and field data have been sufficiently cleaned up, now we need to join the
# two datasets together by station and date
rm(list=ls())
library(tidyverse)
library(lubridate)

buoy <- read_csv(file.path("05_data_working", "buoy_data_all.csv"))
s2 <- read_csv(file.path("05_data_working", "s2_observations.csv"))

# Add mdy cols to s2 for easier joining
s2 <- s2 %>%
    mutate(year = year(measure_date),
           month = month(measure_date),
           day = day(measure_date),
           station = recode(station,
               WE2 = "WE02",
               WE4 = "WE04",
               WE8 = "WE08",
               WE13 = "WE13"
           )) %>%
    select(-measure_date)


joined <- inner_join(buoy, s2, by=c("year", "month", "day", "station"))
#write_csv(joined, file.path("05_data_working", "s2_buoy_joined.csv"))

# Data from the c6 instrument is almost entirely missing
joined <- joined %>%
    select(-contains("c6"))

ggplot(joined, aes(phycocyanin_rfu, B4)) +
    geom_point(aes(color=I(rgb(B4, B3, B2))), size=4)

# Since chlorophyll is in RFU, maybe we need to rescale by each station so their
# distributions are more similar
joined <- joined %>%
    group_by(station) %>%
    mutate(chlorophylla_rfu = scale(chlorophylla_rfu),
           phycocyanin_rfu = scale(phycocyanin_rfu),
           flourescent_dissolved_organic_matter_rfu = scale(flourescent_dissolved_organic_matter_rfu),
           ca_sed_norm = chlorophylla_rfu / turbidity_ntu)

ggplot(joined, aes(ca_sed_norm, B4)) +
    geom_point(aes(color=I(rgb(B4, B3, B2))), size=4)