"
NOAA HAB monitoring data from Lake Erie is split by station and year across multiple files. This script
joins all the files together so we get one table of all stations and years.
"

rm(list=ls())
library(tidyverse)
library(tools)
library(janitor)

# Default read_csv function gets a couple things wrong, these files get used
# a lot so standardize how they get read with a function
parse_field_csv <- function(path){
    # Read the file, throw out everything sampled on the bottom of the lake
    ret <- read_csv(path) %>% clean_names() %>%
        filter(sample_depth_category == 'Surface')
    # Fails to parse micro symbol, so rename the cols
    names(ret) <- sapply(names(ret), function(x) {str_replace(x, 'u_fffd_', 'u')})
    # Renaming on bad unicode characters breaks the temperature label, fix that
    # ret <- rename(ret, 
    #                sample_temperature_c = sample_temperature_uc,
    #                ctd_temperature_c = ctd_temperature_uc,
    #                ctd_transmission_pct = ctd_tramission_percent) # NOAA can't spell
    
    # Not everything is parsed as a number correctly, force conversions where they exist
    to_convert <- c("wind_speed_knots", "secchi_depth_m", "particulate_microcystin_ug_l",
                    "dissolved_microcystin_ug_l", "extracted_phycocyanin_ug_l",
                    "soluble_reactive_phosphorus_ug_p_l", "ammonia_ug_n_l",
                    "nitrate_nitrite_mg_n_l", "urea_ug_n_l") %>%
        Filter(x=., f=function(x){x %in% names(ret)})
    
    ret <- ret %>%
        mutate_at(.vars=to_convert, .funs = function(x) {parse_number(as.character(x), na=c("", "NA", 'n/a', 'nd'))})
    
    return(ret)
}

field_paths = c("01_data_in\\erie_hab_monitoring\\field\\lake_erie_habs_field_sampling_results_2012_2018_v2.csv",
                "01_data_in\\erie_hab_monitoring\\field\\lake_erie_habs_field_sampling_results_2019.csv")

field_all <- bind_rows(sapply(field_paths, parse_field_csv)) %>%
    rename(sample_temperature_c = sample_temperature_uc,
           ctd_temperature_c = ctd_temperature_uc,
           ctd_transmission_pct = ctd_tramission_percent) # NOAA can't spell

write_csv(field_all, "05_data_working\\field_obs_2012_2019.csv")
