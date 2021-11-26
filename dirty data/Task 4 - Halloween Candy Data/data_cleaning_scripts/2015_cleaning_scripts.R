


library(tidyverse)
library(here)
library(readxl)
library(janitor)

data_2015 <- read_excel(here(
              "raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))



clean_2015 <- data_2015 %>% 
  clean_names() %>% 
  mutate(id = row_number(),
         year = "2015") %>% 
  unite("year_id", year, id) %>% 
  pivot_longer(c(4:15, 17:37, 39:62, 64:93, 96, 114, 115),
               names_to = "sweets",
               values_to = "rating") %>% 
  rename("age" = "how_old_are_you",
         "trick_or_treating" = 
           "are_you_going_actually_going_trick_or_treating_yourself") %>% 
  mutate(gender = NA_character_) %>% 
  mutate(country = NA_character_) %>%
  select(year_id, age, gender, trick_or_treating, sweets, 
         rating, country) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age > 0 & age < 120)


write_csv(clean_2015, "clean_data/2015_clean_data.csv")