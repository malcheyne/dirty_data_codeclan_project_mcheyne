

library(tidyverse)
library(here)
library(readxl)
library(janitor)

data_2017 <- read_excel(here(
  "raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))

clean_2017 <- data_2017 %>% 
  clean_names() %>% 
  mutate(id = row_number(),
         year = "2017") %>% 
  unite("year_id", year, id) %>% 
  pivot_longer(7:109,
               names_to = "sweets",
               values_to = "rating") %>% 
  rename("age" = "q3_age",
         "gender" = "q2_gender",
         "country"= "q4_country",
         "trick_or_treating" = "q1_going_out") %>% 
  select(year_id, age, gender, trick_or_treating, sweets, 
         rating, country) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age > 0 & age < 120)

write_csv(clean_2017, "clean_data/2017_clean_data.csv")