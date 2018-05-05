library(stringr)
library(tidyverse)
library(VIM)
library(jsonlite)
library(broom)

### food data
yelp <- stream_in(file("yelp-data/business.json"), flatten = TRUE)

# clean up categories data, filter by restaurants
yelp <- yelp %>% 
  mutate(categories = str_remove(categories,'c\\("'), 
         categories = str_remove(categories, '"\\)'),
         categories = str_replace_all(categories, '", "', ","),
         categories = str_to_lower(categories))

write_csv(yelp, "data/yelp.csv")

food <- yelp %>% 
  filter(str_detect(categories, "food|restaurants")) %>% 
  mutate(categories = str_remove(categories, "restaurants,"),
       categories = str_remove(categories, ",restaurants"),
       categories = str_remove(categories, "food,"),
       categories = str_remove(categories, ",food"))

food %>% 
  select(1,14:94) %>% 
  rename_at(vars(2:82), funs(str_remove(.,"attributes."))) %>% 
  select(-starts_with("Hair"), -AcceptsInsurance) %>% 
  write_csv("data/food-attributes.csv")

food %>%
  select(1,95:101) %>% 
  write_csv("data/food-hours.csv")

food %>%
  select(1:13) %>% 
  write_csv("data/food.csv")
