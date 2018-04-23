library(stringr)
library(tidyverse)
library(VIM)
library(jsonlite)
library(broom)
yelp <- stream_in(file("yelp-data/business.json"), flatten = TRUE)

# clean up categories data, filter by restaurants
yelp <- yelp %>% 
  mutate(categories = str_remove(categories,'c\\("'), 
         categories = str_remove(categories, '"\\)'),
         categories = str_replace_all(categories, '", "', ", "),
         categories = str_to_lower(categories))

write_csv(yelp, "data/yelp.csv")

food <- yelp %>% 
  filter(str_detect(categories, "food | restaurants"))

write_csv(food, "data/food.csv")

# make smaller table
food_small <- food %>% 
  select(name, neighborhood, address, city, state, postal_code, latitude, longitude, stars, review_count, categories) 

write_csv(food_small, "data/food-small.csv")
