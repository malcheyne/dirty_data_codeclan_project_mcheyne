

library(tidyverse)
library(here)
library(readxl)
library(janitor)

data_2016 <- read_excel(here(
  "raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))

clean_2016 <- data_2016 %>% 
  clean_names() %>% 
  mutate(id = row_number(),
         year = "2016") %>% 
  unite("year_id", year, id) %>% 
  pivot_longer(7:106,
               names_to = "sweets",
               values_to = "rating") %>% 
  rename("age" = "how_old_are_you",
         "gender" = "your_gender",
         "country"= "which_country_do_you_live_in",
         "trick_or_treating" = 
           "are_you_going_actually_going_trick_or_treating_yourself") %>% 
  select(year_id, age, gender, trick_or_treating, sweets, 
         rating, country) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age > 0 & age < 120)

write_csv(clean_2016, "clean_data/2016_clean_data.csv")