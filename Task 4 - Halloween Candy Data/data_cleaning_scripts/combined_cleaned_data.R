

library(tidyverse)
library(here)

clean_2015 <- read_csv(here("clean_data/2015_clean_data.csv"))
clean_2016 <- read_csv(here("clean_data/2016_clean_data.csv"))
clean_2017 <- read_csv(here("clean_data/2017_clean_data.csv"))


clean_15_16 <- bind_rows(clean_2015, clean_2016)

combined_cleaned_data <- bind_rows(clean_15_16, clean_2017)

write_csv(combined_cleaned_data, "clean_data/combined_cleaned_data.csv")