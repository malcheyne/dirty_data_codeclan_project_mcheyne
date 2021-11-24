


library(tidyverse)
library(janitor)
library(readxl)
library(here)

bird_data <- read_excel(here("data/seabirds.xls"), 
                             sheet = "Bird data by record ID")
ship_data <- read_excel(here("data/seabirds.xls"), 
                             sheet = "Ship data by record ID")


names(bird_data)
names(ship_data)


bird_ship_data <- full_join(bird_data, ship_data, by="RECORD ID")

head(bird_ship_data, 10)


clean_b_s <- bird_ship_data %>% 
  clean_names() %>% 
  rename("common_name" =   # bird data
           "species_common_name_taxon_age_sex_plumage_phase",
         "scientific_name" = 
           "species_scientific_name_taxon_age_sex_plumage_phase",
         "total_sighting" = "count",
         "num_feeding" = "nfeed",
         "feeding" = "ocfeed",
         "num_on_water" = "nsow",
         "on_water" = "ocsow",
         "num_on_ice" = "nsoice",
         "on_ice" = "ocsoice",
         "on_ship" = "ocsoshp",
         "in_hand" = "ocinhd",
         "num_fly_by" = "nflyp",
         "fly_by" = "ocflyp",
         "num_group_sighting" = "nacc",
         "group_sighting" = "ocacc",
         "num_ship_wake" = "nfoll",
         "ship_wake" = "ocfol",
         "molting" = "ocmoult",
         "nat_feeding" = "ocnatfed",
         # ship data
         "hemisphere" = "ew",
         "ship_activity" = "sact",
         "speed_knots" = "speed",
         "ship_direc" = "sdir",
         "cloud_cover" = "cld",
         "rain" = "prec",
         "wind_speed" = "wspeed",
         "wind_direc" = "wdir",
         "air_temp" = "atmp",
         "air_press" = "aprs",
         "sea_state" = "sste",
         "sea_temp" = "stmp",
         "salinity" = "sal",
         "obser" = "obs",
         "census_len" = "csmeth",
         "obser" = "obs"
  ) %>% 
  mutate(feeding  = 
           recode(feeding , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         on_water  = 
           recode(on_water , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         on_ice  = 
           recode(on_ice , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         on_ship  = 
           recode(on_ship , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         in_hand  = 
           recode(in_hand , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         fly_by  = 
           recode(fly_by , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         group_sighting  = 
           recode(group_sighting , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         ship_wake  = 
           recode(ship_wake , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         molting  = 
           recode(molting , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         nat_feeding  = 
           recode(nat_feeding , "N" = "NO", "Y" = "YES", "U" = "UNKNOWN"),
         hemisphere  = 
           recode(hemisphere , "E" = "East", "W" = "West"),
         census_len  = 
           recode(census_len , "F" = "> 10 min", "P" = "< 10 min",)
  ) %>% 
  mutate(
    common_name = if_else(str_detect(common_name, 
                                     "\\[NO BIRDS RECORDED\\]"), 
                                        NA_character_, 
                                          common_name),
    common_name = if_else(str_detect(common_name, 
                                     "[A-Z]{2}.*$"), 
                          str_replace_all(common_name, 
                                          "[A-Z]{2}.*$", ""), 
                                            common_name),
    common_name = if_else(str_detect(common_name, 
                                     "sensu"),
                          str_replace_all(common_name, 
                                          "sensu", ""), 
                                            common_name),
    common_name = if_else(str_detect(common_name, 
                                     "lato"),
                          str_replace_all(common_name, 
                                          "lato", ""), 
                                            common_name),
    common_name = if_else(str_detect(common_name, 
                                     "\\([a-z].*\\)$"),
                          str_replace_all(common_name, 
                                          "\\([a-z].*\\)$", 
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Shy \\/.*$"),
                          str_replace_all(common_name, 
                                          "\\/.*$", ""), 
                                            common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Skua \\(unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(unidentified\\)",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Westland / White-chinned petrel"),
                          str_replace_all(common_name, 
                                          " \\/.*$",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Great-winged / Grey-faced petrel"),
                          str_replace_all(common_name, 
                                          " \\/.*$",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Jaeger \\(unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(unidentified\\) ",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Albatross \\(unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(unidentified\\) ",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Mollymawk \\(unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(unidentified\\) ",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Seabird \\(Unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(Unidentified\\)",
                                            ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Booby \\(unidentified\\)"),
                          str_replace_all(common_name, 
                                          " \\(unidentified\\) ",
                                           ""), common_name),
    common_name = if_else(str_detect(common_name, 
                                     "Lesser frigatebird M"),
                          str_replace_all(common_name, 
                                          " M",
                                           ""), common_name)
  ) %>% 
  mutate(common_name = str_trim(common_name, side = "both"))


clean_b_s

write_csv(clean_b_s, "data/seabirds_cleaned_data.csv")