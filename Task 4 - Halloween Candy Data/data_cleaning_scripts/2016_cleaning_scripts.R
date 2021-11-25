

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
  pivot_longer(c(7:11, 13:42, 44:77, 80:100, 102, 103, 106),
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

country_clean_2016 <- clean_2016 %>% 
  mutate(country = str_trim(country, side = "both"),    # changing USA to US
         country = str_squish(country),
         country = str_replace_all(country, 
                                   "[uU][sS][aA][ :punct:]*", "US"),
         country = str_replace_all(country, 
                                   "[uU][sS][:punct:]*", "US"),
         country = str_replace_all(country, 
                                   "US US", "US"),
         country = str_replace_all(country, 
                                   "USUSUSUS", "US"),
         country = str_replace_all(country, 
                                   "USUSUS", "US"),
         country = str_replace_all(country, 
                                   "[uU][:punct:]*[sS][:punct:]*", "US"),
         country = str_replace_all(country, 
                       "[uU][ :punct:]*[sS][ :punct:]*[aA][ :punct:]*", "US"),
         country = str_replace_all(country, "[uU][sS][:punct:]*", "US"),
         country = str_replace_all(country, "[uU]nited +[sS]tates", "US"),
         country = str_replace_all(country, "of [aA]merica", "US"),
         country = str_replace_all(country, "[aA]merica", "US"),
         country = str_replace_all(country, " [uU][sS]", "US"),
         country = str_replace_all(country, "Murica", "US"),
         country = str_replace_all(country, "Merica", "US"),
         country = str_replace_all(country, "[:punct:]*[uU][sS]", "US"),
         country = str_replace_all(country, "USUS", "US"),
         country = str_replace_all(country, "USSA", "US"),
         country = str_replace_all(country, 
                                   "The Yoo Ess of Aaayyyyyy", "US"),
         country = str_replace_all(country, 
                                   "Sub-Canadian NorthUS... US", "US"),
         country = str_replace_all(country, "the best one US", "US"),
         country = str_replace_all(country, 
                                   "USI [ :punct:a-zA-Z:punct:a-zA-Z]*", "US"),
         country = str_replace_all(country, "United Sates", "US"),
         country = str_replace_all(country, "United State", "US"),
         country = str_replace_all(country, "UNited States", "US"),
         country = str_replace_all(country, "United Stetes", "US"),
         country = str_replace_all(country, "Units States", "US"),
         country = str_replace_all(country, "Trumpistan", "US"),
         country = str_replace_all(country, 
                         "\\'s an election year so who can really tell\\)", "")
  ) %>% 
  mutate(country = str_trim(country, side = "both"),    # Change to Uk
         country = str_squish(country),
         country = str_replace_all(country, "[eE]ngland", "UK"),
         country = str_replace_all(country, "[uU][kK]", "UK"),
         country = str_replace_all(country, "United Kindom", "UK"),
         country = str_replace_all(country, "[uU]nited [kK]ingdom", "UK")
  ) %>% 
  mutate(country = str_trim(country, side = "both"),    # Change to Canada
         country = str_squish(country),
         country = str_replace_all(country, "[cC]anada", "Canada")
  ) %>% 
  mutate(country = str_replace_all(country, 
                               "[0-9]{2}[:punct:][0-9]", "Unknown Country"),
         country = str_replace_all(country, 
                   "A tropical island south of the equator", "Unknown Country"),
         country = str_replace_all(country, "Denial", "Unknown Country"),
         country = str_replace_all(country, "god's country", "Unknown Country"),
         country = str_replace_all(country, 
                                   "one of the best ones", "Unknown Country"),
         country = str_replace_all(country, "See above", "Unknown Country"),
         country = str_replace_all(country, "Somewhere", "Unknown Country"),
         country = str_replace_all(country, 
                             "there isn't one for old men", "Unknown Country"),
         country = str_replace_all(country, "this one", "Unknown Country")
  ) %>% 
  mutate(country = if_else(str_detect(country, "US") |# change to rest of world
                             str_detect(country, "UK") |
                             str_detect(country, "Canada") |
                             str_detect(country, "Unknown Country") |
                             is.na(country),
                           country, "all other countries")
  ) %>%
  mutate(country =  str_replace_all(country, 
                                "Not theUSor Canada", "all other countries"),
         country =  str_replace_all(country, 
                                    "AUStria", "all other countries"),
         country =  str_replace_all(country, 
                                    "AUStralia", "all other countries")
  )

write_csv(country_clean_2016, "clean_data/2016_clean_data.csv")