Final Project
================
kimchi-stew
May 4th, 2018

### Load Packages & Data

``` r
library(stringr)
library(stringi)
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ readr   1.1.1
    ## ✔ tibble  1.4.2     ✔ purrr   0.2.4
    ## ✔ tidyr   0.8.0     ✔ dplyr   0.7.4
    ## ✔ ggplot2 2.2.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(broom)
library(splitstackshape)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rlist)
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(miscTools)
library(ggplot2)
library(ggmap)
library(maps)
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(mapdata)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
set.seed(415)
# run load-data.R first
food <- read_csv("data/food.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   business_id = col_character(),
    ##   name = col_character(),
    ##   neighborhood = col_character(),
    ##   address = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   postal_code = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   stars = col_double(),
    ##   review_count = col_integer(),
    ##   is_open = col_integer(),
    ##   categories = col_character()
    ## )

``` r
food_attr <- read_csv("data/food-attributes.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_logical(),
    ##   business_id = col_character(),
    ##   RestaurantsPriceRange2 = col_integer(),
    ##   Alcohol = col_character(),
    ##   NoiseLevel = col_character(),
    ##   RestaurantsAttire = col_character(),
    ##   WiFi = col_character(),
    ##   Smoking = col_character(),
    ##   BYOBCorkage = col_character(),
    ##   AgesAllowed = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
food_hours <- read_csv("data/food-hours.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   business_id = col_character(),
    ##   hours.Friday = col_character(),
    ##   hours.Tuesday = col_character(),
    ##   hours.Thursday = col_character(),
    ##   hours.Wednesday = col_character(),
    ##   hours.Monday = col_character(),
    ##   hours.Saturday = col_character(),
    ##   hours.Sunday = col_character()
    ## )

``` r
food_attr <- food_attr %>% 
  select(-starts_with("DietaryRestrictions"))

food_attr_col <- colnames(food_attr) %>% 
  as.data.frame() %>%
  mutate(cols = str_replace(., "-", "_")) %>% 
  select(-.) %>% 
  pull()

colnames(food_attr) <- food_attr_col
```

For our final project, we worked with a Yelp dataset. Yelp is a web and
mobile platform that publishes crowd-sourced reviews about local
businesses, as well as online reservation service through Yelp
reservations. The data we used was released by the Yelp Dataset
Challenge to encourage students to conduct research and analysis. The
scope of the dataset is extremely wide including business information,
reviews and user data. Even the business subset contains information
about 174,000 businesses and over 1.2 million business attributes, such
as workday hours, parking, and ambience. Given this huge dataset, we
decided to narrow down our interests and perform analysis specifically
on the restaurant data. We were interested in exploring what attribute
contribute to high restaurant star ratings on Yelp. Eventually, we
wanted to construct a regression model that predicts the star ratings of
restaurants based on their attributes.

We started by converting dataset from JSON form to CSV files. The R
package jsonlite was used to stream in the json file and flatten it. The
conversion happened in the load-data R script, in which we also cleaned
up the data using functions from the stringr package. In addition, the
data was split into multiple, csv subsets that can later be joined by
the common variable, business\_id. 1) Food.csv (general business
information): 69070 observations and 13 variables, including name, city,
state, longitude, latitude, categories, etc. 2) Food\_attributes.csv
(business attributes information): 69070 observations and 66 variables,
including GoodForKids, RestaurantsAttire, WiFi, etc. 3) Food\_hours.csv
(business hour information): 69070 observations and 8 variables,
including hours.Monday, hours.Tuesday, etc.

Before building our linear model, we created rudimentary visualizations
to get a sense of the overall trend of the data.

``` r
food_stars <- food %>%
  count(stars) %>% 
  arrange(desc(stars)) 

ggplot(data = food_stars,aes(x = stars,y= n)) +
  geom_bar(stat = "identity",fill = "steelblue")+
  labs(title = "Count of of star ratings",y = "count")+
  geom_text(aes(label = n),position = "stack",vjust = 0) +
  scale_x_continuous(breaks = c(1,1.5,2,2.5,3,3.5,4,4.5,5))
```

![](project_files/figure-gfm/initial_visualizations-1.png)<!-- -->

``` r
food %>%
  summarise(mean = mean(stars),median = median(stars))
```

    ## # A tibble: 1 x 2
    ##    mean median
    ##   <dbl>  <dbl>
    ## 1  3.50   3.50

The first bar graph showed the counts of each star rating categories.
It’s important to note that star ratings on Yelp are not continuous,
but discrete, and the ratings change by increments of 0.5. Therefore, we
could not approximate the ratings with a normal distribution. However,
we did know from summary statistics that both mean and median star
ratings are approximately 3.5. Additionally, we should consider the
possible anchoring effect that one review could have on another. Reviews
are not random like throwing dice and adding up the sum. Both the nature
of review ratings and the discrete system implemented by Yelp should be
considered when evaluating the validity and predictability of our
multiple regression linear model.

We also did a simple map to visualize the locations of the restaurants.
This is because the Yelp website specified that there are 11
metropolitan areas within this dataset but we were seeing many more
cities than 11. Therefore we just wanted to create a visualization to
see all restaurant locations.

``` r
usa <- map_data("usa")
world_map <- map_data("world")
p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

#Add map to base plot
base_world_messy <- p + geom_polygon(data=world_map, aes(x=long, y=lat, group=group), 
                               colour="black", fill="black")
cleanup <- 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

base_world <- base_world_messy + cleanup

food %>% 
  filter(longitude > 100)
```

    ## # A tibble: 1 x 13
    ##   business_id  name  neighborhood address city  state postal_code latitude
    ##   <chr>        <chr> <chr>        <chr>   <chr> <chr> <chr>          <dbl>
    ## 1 Zmp2_b2gpSl… Terr… <NA>         Green … Hend… NV    89052           36.0
    ## # ... with 5 more variables: longitude <dbl>, stars <dbl>,
    ## #   review_count <int>, is_open <int>, categories <chr>

``` r
food %>% 
  filter(longitude < -130) 
```

    ## # A tibble: 1 x 13
    ##   business_id  name  neighborhood address city  state postal_code latitude
    ##   <chr>        <chr> <chr>        <chr>   <chr> <chr> <chr>          <dbl>
    ## 1 1yQUqh3_h1I… TriB… <NA>         88 Bru… Edin… EDH   EH10 4HG        90.0
    ## # ... with 5 more variables: longitude <dbl>, stars <dbl>,
    ## #   review_count <int>, is_open <int>, categories <chr>

``` r
food <- food %>% 
  filter(longitude < 100 & longitude >-130) 
  #%>% 
  #filter(business_id != "vBxK_MAGuy8eWL_CCfUCUQ") %>% 
  #filter(latitude > -20 | latitude < -45)

map_data <- 
  base_world +
  geom_point(data=food, 
             aes(x=longitude, y=latitude), colour="red", 
             fill="Pink",pch=21, size=5, alpha=I(0.5))
map_data
```

![](project_files/figure-gfm/map-1.png)<!-- -->

The map graph showed that there are indeed many more cities included in
the dataset but the majority of cities are in North American and Europe.

``` r
### this block counts up non-null values 

# create new var to represent if at least one of the subvalues is true
food_attr_temp <- food_attr %>% 
  select(-business_id) %>% 
  mutate(BusinessParking =  BusinessParking.garage    == TRUE   |       
                            BusinessParking.street    == TRUE |             
                            BusinessParking.validated   == TRUE |    
                            BusinessParking.lot       == TRUE | 
                            BusinessParking.valet     == TRUE,
         Music =  Music.dj                == TRUE |             
                  Music.background_music    == TRUE |           
                  Music.no_music              == TRUE |     
                  Music.karaoke           == TRUE |         
                  Music.live              == TRUE | 
                  Music.video             == TRUE |         
                  Music.jukebox           == TRUE , 
         Ambience = Ambience.romantic   == TRUE |           
                    Ambience.intimate   == TRUE |           
                    Ambience.classy     == TRUE |       
                    Ambience.hipster    == TRUE |           
                    Ambience.divey      == TRUE |       
                    Ambience.touristy   == TRUE |           
                    Ambience.trendy     == TRUE |       
                    Ambience.upscale  == TRUE |
                    Ambience.casual     == TRUE,    
         BestNights = BestNights.monday     == TRUE |   
                      BestNights.tuesday        == TRUE |           
                      BestNights.friday         == TRUE |       
                      BestNights.wednesday  == TRUE |               
                      BestNights.thursday       == TRUE |           
                      BestNights.sunday         == TRUE |       
                      BestNights.saturday       == TRUE,    
         GoodForMeal =  GoodForMeal.dessert   == TRUE |     
                        GoodForMeal.latenight   == TRUE |
                        GoodForMeal.lunch         == TRUE |         
                        GoodForMeal.dinner      == TRUE |           
                        GoodForMeal.breakfast   == TRUE |       
                        GoodForMeal.brunch      == TRUE)    

# recode null and non null with 0/1
food_attr_temp[is.na(food_attr_temp)] <- 0
food_attr_temp[food_attr_temp != 0] <- 1

# sum up to get non null values
food_attr_counts <- food_attr_temp %>% 
  summarise_all(funs(sum(. == 1))) %>% 
  gather(attribute, total, 1:ncol(.)) %>% 
  arrange(desc(total))

# for categories broken into multiple columns, get frequency of occurences
level_props <- data.frame(attribute = colnames(food_attr)) %>% 
  filter(str_detect(attribute, "\\.")) %>% 
  inner_join(food_attr_counts, by = "attribute") %>% 
  mutate(attribute_parent = str_remove(attribute, "\\..+"),
         attribute = str_remove(attribute, ".+\\.")) %>%
  rename(subtotal = total) %>% 
  inner_join(food_attr_counts, by = c("attribute_parent" = "attribute")) %>% 
  mutate(prop = subtotal / total) %>% 
  mutate(subtotal = case_when(attribute == "hipster" ~ as.double(858),
                              attribute != "hipster" ~ as.double(subtotal))) %>% 
  arrange(attribute_parent, desc(prop)) 
```

    ## Warning: Column `attribute` joining factor and character vector, coercing
    ## into character vector

``` r
# manually break tie
level_props[4,2] <- 858
level_props[4,5] <- 0.03737

# for these multi-column attributes, this tells us how many columns are usually true in any row
data.frame(attribute = colnames(food_attr)) %>% 
  filter(str_detect(attribute, "\\.")) %>% 
  inner_join(food_attr_counts, by = "attribute") %>% 
  mutate(attribute_parent = str_remove(attribute, "\\..+")) %>%
  rename(subtotal = total) %>% 
  inner_join(food_attr_counts, by = c("attribute_parent" = "attribute")) %>% 
  mutate(prop = subtotal / total) %>% 
  arrange(attribute_parent, desc(prop)) %>% 
  group_by(attribute_parent) %>% 
  summarise(undisjoint_factor = sum(subtotal) / median(total))
```

    ## Warning: Column `attribute` joining factor and character vector, coercing
    ## into character vector

    ## # A tibble: 5 x 2
    ##   attribute_parent undisjoint_factor
    ##   <chr>                        <dbl>
    ## 1 Ambience                      1.11
    ## 2 BestNights                    2.53
    ## 3 BusinessParking               1.11
    ## 4 GoodForMeal                   1.71
    ## 5 Music                         1.23

To make our analysis more efficient we had cut down and reformat our
code. The issues we had to solve for were the following: aggregate
related columns by some protocol and find the subset of columns that
maximize non-null rows and number of
attributes.

``` r
### this code block merges the multi column attribues into single columns
### and if there are multiple values follows a protocol (min, max, or prob) 
### to determine which value should represent the row
### e.g. the merged BestNights column might have `wednesday,friday,sunday`,
### and if `wednesday` appears least in the dataset, the min protocol 
### would reduce this to `wednesday`

# get the names of the multi-column attributes
lst <- food_attr %>% 
  select(contains(".")) %>% 
  colnames() 

# this function changes the formatting of the multicolumn attributes 
# true values take on the name of the column, false values are periods
convert_dot <- function(col_name) {
  col_name_short <- str_remove(col_name, ".+\\.")
  index <- grep(col_name, colnames(food_attr))
  col <- food_attr[index]
  col[col == TRUE] <- paste0(col_name_short, ",")
  col[col == FALSE] <- "."
  return(col)
}

lst <- food_attr %>% 
  select(contains(".")) %>% 
  colnames() 


food_attr_convert <- purrr::map(lst, convert_dot) %>% 
  as.data.frame()

# aggregate the columnn by pasting together the variable values;
# since true values are the name of the column, this column tells us the names
# of the sub attributes that are true... formatting at the end
food_attr_convert <- food_attr_convert %>% 
  mutate(aggBusinessParking =  paste0(BusinessParking.garage,       
                                      BusinessParking.street,           
                                      BusinessParking.validated,     
                                      BusinessParking.lot,
                                      BusinessParking.valet, sep=""),
         aggMusic = paste0(Music.dj,                
                           Music.background_music,      
                           Music.no_music,
                           Music.karaoke    ,
                           Music.live,
                           Music.video,         
                           Music.jukebox, sep=""),
         aggAmbience = paste0(Ambience.romantic,
                              Ambience.intimate,        
                              Ambience.classy   ,   
                              Ambience.hipster,     
                              Ambience.divey    ,   
                              Ambience.touristy,        
                              Ambience.trendy   ,   
                              Ambience.upscale,
                              Ambience.casual, sep=""),
         aggBestNights = paste0(BestNights.monday    ,  
                                BestNights.tuesday      ,   
                                BestNights.friday           ,
                                BestNights.wednesday    ,       
                                BestNights.thursday     ,   
                                BestNights.sunday           ,
                                BestNights.saturday, sep=""),
         aggGoodForMeal = paste0(GoodForMeal.dessert,       
                                 GoodForMeal.latenight,
                                 GoodForMeal.lunch        , 
                                 GoodForMeal.dinner     ,   
                                 GoodForMeal.breakfast  ,
                                 GoodForMeal.brunch, sep="")) %>% 
  select(-contains(".")) %>% 
  mutate_all(funs(str_replace_all(.,",+", ","))) %>% 
  mutate_all(funs(str_replace_all(., "[NA]+", "NA"))) %>% 
  mutate_all(funs(str_remove_all(., "\\.NA|NA\\."))) %>% 
  mutate_all(funs(str_replace_all(., "^\\.+$", "None"))) %>% 
  mutate_all(funs(str_remove_all(., "\\.+"))) %>% 
  mutate_all(funs(str_remove(.,"^,"))) %>% 
  mutate_all(funs(str_remove(.,",$")))

# turn "NA" into NA
food_attr_convert[food_attr_convert == "NA"] <- NA

### PROBLEM: too many collisions between variables - need to reduce to one value; don't want to eliminate too much information
### SOLUTION1: choose in order of frequency, low to high
### IMPLEMENTATION: iterate through, call function: if (numberofcommas + 1) > 1: split into vector, for each element in vector
### PROBLEM: if all slightly equal numbers but still collisions, some vals might not get seen

# min protocol: picks the least frequent value
pick_index_min <- function(freq_vect) {
  return(grep(min(freq_vect), freq_vect))
}

# max protocol: picks the most frequent value
pick_index_max <- function(freq_vect) {
  return(grep(max(freq_vect), freq_vect))
}

# prob protocol: picks the value probabilistically according to frequency so that
# same combinations of values aren't always the same
pick_index_prob <- function(freq_vect) {
  ord_vect <- sort(freq_vect)
  total <- reduce(freq_vect, sum)
  ord_vect_prob <- sapply(ord_vect, function(x) x/total)
  rev_vect_prob <- sort(ord_vect_prob, decreasing = TRUE)
  for (i in 2:length(rev_vect_prob)){
    rev_vect_prob[[i]] <- rev_vect_prob[[i]] + rev_vect_prob[[i-1]]
  }
  rand <- runif(1)
  ind <- which(sapply(rev_vect_prob, function (x) x > rand))[[1]]
  return(grep(ord_vect[[ind]],freq_vect))
}

# this function reduces a row of multiple values to a single value
# according to some protocol, in this case min
my_reduce <- function(col) {

  for (i in 1:length(col)) {
    val_list <- col[[i]]
    if (is.na(val_list) | val_list == "None" | !str_detect(val_list, ",")) {
      col[[i]] <- val_list
    } else {
      vals <- unlist(str_split(val_list, ","))
      freq_vect <- c()
      for (val in vals) {
        freq <- level_props %>%
          filter(attribute == val) %>%
          select(subtotal) %>%
          pull()
        freq_vect <- c(freq_vect, freq)
      }
      index <- pick_index_min(freq_vect)
      col[[i]] <- vals[index]
    }
  }
  return(col)
}

# apply the function
food_attr_reduce <- food_attr_convert %>% 
  mutate_all(funs(my_reduce))
```

``` r
convert <- function(col_name, tbl) {
  index <- grep(col_name, colnames(tbl))
  col <- tbl[index]
  col[!is.na(col)] <- paste0(col_name, ",")
  col[is.na(col)] <- ""
  return(col)
}

column_list <- food_attr_temp %>% 
  select(1:65) %>% 
  select(-contains(".")) %>% 
  summarise_all(funs(sum(. == 1))) %>% 
  gather(attribute, total, 1:ncol(.)) %>% 
  arrange(desc(total)) %>% 
  filter(total > 10000) %>% 
  pull(attribute)

test_df <- food_attr[c(column_list)] %>% 
  cbind(food_attr_reduce)

test_columns <- purrr::map(colnames(test_df), function(x) convert(x,food_attr)) %>% 
  as.data.frame() %>% 
  unite(attr,sep = "", remove = TRUE) %>% 
  count(attr) %>% 
  mutate(num_attr = str_count(attr, ","),
          opt_score = num_attr*n) %>% 
  filter(opt_score > 0) %>% 
  arrange(desc(num_attr * n)) %>% 
  pull(attr) %>% 
  sapply(function(x) unlist(strsplit(x, split=",")))
```

For the first problem, there were 6 columns that were initially broken
up across around 30 columns. For example, the column GoodForMeal was
initially split up as a column for every meal encoded as true or false,
since a restaurant could be good for multiple meals. We needed to
aggregate these values into a single column, but we had to figure out
how to handle collisions when two or more columns were true. We created
a function to reduce these aggregated columns, replacing their list of
values with the value that occurs most in the dataset.

``` r
cols_eval <- function(col_names, tbl) {
  food_attr_local <- tbl[col_names]
  output <- food_attr_local %>% 
    drop_na() %>% 
    summarise(n = nrow(.),
              attr = paste0(col_names, collapse = ",")) %>% 
    mutate(num_attr = length(col_names),
           opt_score = n*num_attr)
  
  return(output)
}

powerset <- function(vect) {
  total <- character()
  range <- c(((6 * length(vect)) %/% 10):length(vect))
  print(range)
  for (m in range) {
    x <- combn(vect, m, simplify = FALSE)
    total <- append(total,x)
  }
  return(total)
}

find_best_cols <- function(tbl) {
  #pwrset <- powerset(colnames(tbl))
  result <- map_df(test_columns, function(x) cols_eval(x,tbl)) %>%               
    arrange(desc(opt_score)) %>% 
    select(attr, opt_score, n, num_attr)
  return(result)
}

all_cols <- find_best_cols(test_df[1:22])
all_cols %>%
  ggplot(aes(x = num_attr, y = n, z = opt_score, color = (n*num_attr))) +
  geom_point(size = 4, alpha = 1) +
  stat_function(fun = function(x) 412555/(.5*x), color = "red") +
  stat_function(fun = function(x) 412555/x, color = "red") +
  stat_function(fun = function(x) 412555/(2*x), color = "red") +
  stat_function(fun = function(x) 412555/(4*x), color = "red") +
  stat_function(fun = function(x) 412555/(8*x), color = "red") +
  scale_color_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  ylim(0,75000) +
  labs(color = "Optimization Function", y = "Number of Rows", x = "Number of Attributes", title = "Finding best combination of columns", subtitle = "Optimizing for number of rows and number of attributes")
```

    ## Warning: Removed 63 rows containing missing values (geom_path).

    ## Warning: Removed 29 rows containing missing values (geom_path).

    ## Warning: Removed 11 rows containing missing values (geom_path).

    ## Warning: Removed 3 rows containing missing values (geom_path).

![](project_files/figure-gfm/find-optimal-column-1.png)<!-- -->

``` r
(candidate_columns <- all_cols %>%
  top_n(3, opt_score) %>%
  pull(attr))
```

    ## [1] "RestaurantsPriceRange2,BusinessAcceptsCreditCards,RestaurantsTakeOut,RestaurantsAttire,Alcohol,RestaurantsGoodForGroups,GoodForKids,NoiseLevel,RestaurantsTableService,HasTV,OutdoorSeating,RestaurantsReservations,RestaurantsDelivery"                 
    ## [2] "RestaurantsPriceRange2,BusinessAcceptsCreditCards,RestaurantsTakeOut,RestaurantsAttire,Alcohol,WiFi,RestaurantsGoodForGroups,GoodForKids,NoiseLevel,RestaurantsTableService,HasTV,OutdoorSeating,RestaurantsReservations,RestaurantsDelivery"            
    ## [3] "RestaurantsPriceRange2,BusinessAcceptsCreditCards,RestaurantsTakeOut,RestaurantsAttire,Alcohol,WiFi,RestaurantsGoodForGroups,GoodForKids,NoiseLevel,BikeParking,RestaurantsTableService,HasTV,OutdoorSeating,RestaurantsReservations,RestaurantsDelivery"

Now that we handled that, we had to find the subset of columns that
would maximize the amount of data that we would have. This means
maximizing rows times columns. We created a function that calculates the
amount of data in the subset of some combination of tables. To find the
optimal combination, we would have to try every possible combination of
columns, which is equivalent to the powerset of the columns. And since
we were working with 22 columns. This would be 2^22. So we clearly
couldn’t do that. To get around this we first looked at the possible
combinations of columns that actually existed in the dataset. This cut
down our search space from 2^22 to around 5000. A significant
improvement. From there we calculated the subset of the dataset that
would maximize our data.

``` r
# use the significant columns found to trim down dataset
# this puts together the significant columns we found with our new merged columns, drop NA's
food_attr_mod_2 <- food_attr %>% 
  cbind(food_attr_reduce) %>% 
  select(business_id, BusinessAcceptsCreditCards,RestaurantsPriceRange2,GoodForKids,BikeParking,Alcohol,HasTV,NoiseLevel,RestaurantsAttire,RestaurantsGoodForGroups,Caters,WiFi,RestaurantsReservations,RestaurantsTakeOut, aggBusinessParking, aggAmbience, aggGoodForMeal) %>% 
  drop_na()

food_tall <- food %>% 
  cSplit("categories", direction = "tall", sep = ",") 

category_counts <- food_tall %>% 
  count(categories) %>% 
  arrange(desc(n))

# this function reduces a row of multiple values to a single value
# according to some protocol, in this case max
my_reduce_2 <- function(col) {

  for (i in 1:length(col)) {
    val_list <- col[[i]]
    if (is.na(val_list) | val_list == "None" | !str_detect(val_list, ",")) {
      col[[i]] <- val_list
    } else {
      vals <- unlist(str_split(val_list, ","))
      freq_vect <- c()
      for (val in vals) {
        freq <- category_counts %>%
          filter(categories == val) %>%
          select(n) %>%
          pull()
        freq_vect <- c(freq_vect, freq)
      }
      index <- pick_index_max(freq_vect)
      col[[i]] <- vals[index]
    }
  }
  return(col)
}

# reduce the categories variable
food$categories <- my_reduce_2(food$categories)

# pull the top 50 categories
large_categories_list <- food %>% 
  count(categories) %>% 
  arrange(desc(n)) %>% 
  head(50) %>% 
  pull(categories)

# filter by the top 50 categories
food_reduce <- food %>% 
  filter(categories %in% large_categories_list)

# merge the business info dataset with the business attributes 
food_reduce <- food_reduce %>% 
  inner_join(food_attr_mod_2, by = "business_id")

# turn character columns into factors
cols = c("Alcohol", "NoiseLevel", "RestaurantsAttire", "WiFi", "aggBusinessParking", "aggAmbience", "aggGoodForMeal", "categories")
food_reduce[cols] <- lapply(food_reduce[cols], factor)

# create training set
train_full <- food_reduce %>% 
  head(9*nrow(food_reduce)/10)

# create test set
test_full <- food_reduce %>% 
  tail(1*nrow(food_reduce)/10)
```

With this full dataset, we performed a linear regression using the
following characteristics to predict star score: food within price range
of 11-30(dollars), good for kids, acceptance of credit cards, bike
parking, alcohol sold, TV present, noise level, dress code, fit for
groups, catering ability, WiFi present, accepts reservations, takeout
offered, parking available, ambience, and best mealtime.

``` r
#run linear analysis 
lm_food <- lm(stars ~ RestaurantsPriceRange2 + categories + BusinessAcceptsCreditCards + Alcohol +  HasTV + NoiseLevel +  RestaurantsGoodForGroups + Caters + WiFi + aggBusinessParking + aggAmbience + aggGoodForMeal + review_count + BikeParking + GoodForKids + RestaurantsReservations + RestaurantsTakeOut + RestaurantsAttire + RestaurantsGoodForGroups,data = food_reduce)

summary(lm_food)$r.squared
```

    ## [1] 0.2330068

``` r
lm_food
```

    ## 
    ## Call:
    ## lm(formula = stars ~ RestaurantsPriceRange2 + categories + BusinessAcceptsCreditCards + 
    ##     Alcohol + HasTV + NoiseLevel + RestaurantsGoodForGroups + 
    ##     Caters + WiFi + aggBusinessParking + aggAmbience + aggGoodForMeal + 
    ##     review_count + BikeParking + GoodForKids + RestaurantsReservations + 
    ##     RestaurantsTakeOut + RestaurantsAttire + RestaurantsGoodForGroups, 
    ##     data = food_reduce)
    ## 
    ## Coefficients:
    ##                         (Intercept)               RestaurantsPriceRange2  
    ##                           3.8491982                           -0.0248559  
    ##    categoriesamerican (traditional)               categoriesasian fusion  
    ##                          -0.1638965                           -0.1028240  
    ##                  categoriesbakeries                   categoriesbarbeque  
    ##                           0.2279677                           -0.1328092  
    ##        categoriesbreakfast & brunch                    categoriesbuffets  
    ##                          -0.0186469                           -0.7927418  
    ##                   categoriesburgers                      categoriescafes  
    ##                          -0.4231210                            0.0708344  
    ##            categoriescanadian (new)                  categoriescaribbean  
    ##                           0.0626801                            0.2355521  
    ##             categorieschicken wings                    categorieschinese  
    ##                          -0.2544552                           -0.2477567  
    ##              categoriescoffee & tea                      categoriesdelis  
    ##                           0.1666148                           -0.0860110  
    ##                  categoriesdesserts                     categoriesdiners  
    ##                           0.0585137                           -0.0154982  
    ## categoriesevent planning & services               categoriesfast burgers  
    ##                           0.0325244                           -0.8391822  
    ##                 categoriesfast food               categoriesfast mexican  
    ##                          -0.5950679                           -0.6653847  
    ##           categoriesfast sandwiches                       categoriesfood  
    ##                          -0.6663821                            0.2316108  
    ##                    categoriesfrench                     categoriesgerman  
    ##                           0.1538868                            0.2493182  
    ##                     categoriesgreek                    categoriesgrocery  
    ##                          -0.0966365                            0.3079497  
    ## categoriesice cream & frozen yogurt                     categoriesindian  
    ##                           0.2850597                           -0.0472950  
    ##                   categoriesitalian                   categoriesjapanese  
    ##                          -0.0079859                           -0.0716967  
    ##    categoriesjuice bars & smoothies                     categorieskorean  
    ##                           0.3043973                           -0.1131043  
    ##             categoriesmediterranean                    categoriesmexican  
    ##                           0.1326355                           -0.1236350  
    ##            categoriesmiddle eastern                  categoriesnightlife  
    ##                          -0.0393033                           -0.0656997  
    ##                     categoriespizza                categoriesrestaurants  
    ##                          -0.0903598                           -0.5208047  
    ##                     categoriessalad                 categoriessandwiches  
    ##                           0.1212113                           -0.0036862  
    ##                   categoriesseafood                   categoriesshopping  
    ##                          -0.0686852                            0.4653281  
    ##            categoriesspecialty food                categoriessteakhouses  
    ##                           0.1506302                           -0.1839474  
    ##                categoriessushi bars                       categoriesthai  
    ##                          -0.0576488                            0.0024246  
    ##                categoriesvietnamese             categorieswine & spirits  
    ##                          -0.0815497                            0.0660468  
    ##      BusinessAcceptsCreditCardsTRUE                      Alcoholfull_bar  
    ##                          -0.2714132                           -0.1676520  
    ##                         Alcoholnone                            HasTVTRUE  
    ##                          -0.0417623                            0.0264866  
    ##                      NoiseLevelloud                      NoiseLevelquiet  
    ##                          -0.1620077                            0.1268545  
    ##                 NoiseLevelvery_loud         RestaurantsGoodForGroupsTRUE  
    ##                          -0.3913551                           -0.0487859  
    ##                          CatersTRUE                               WiFino  
    ##                           0.1169467                           -0.0035554  
    ##                            WiFipaid                aggBusinessParkinglot  
    ##                          -0.1203979                            0.0740764  
    ##              aggBusinessParkingNone             aggBusinessParkingstreet  
    ##                          -0.0332294                            0.1676080  
    ##             aggBusinessParkingvalet          aggBusinessParkingvalidated  
    ##                           0.0221258                            0.1223344  
    ##                   aggAmbienceclassy                     aggAmbiencedivey  
    ##                           0.1485267                            0.1379329  
    ##                  aggAmbiencehipster                  aggAmbienceintimate  
    ##                           0.2571216                            0.2664053  
    ##                     aggAmbienceNone                  aggAmbienceromantic  
    ##                          -0.0792281                            0.1448438  
    ##                 aggAmbiencetouristy                    aggAmbiencetrendy  
    ##                          -0.2548824                            0.1641312  
    ##                  aggAmbienceupscale                 aggGoodForMealbrunch  
    ##                           0.2028500                            0.1840162  
    ##               aggGoodForMealdessert                 aggGoodForMealdinner  
    ##                           0.0892061                            0.1971668  
    ##             aggGoodForMeallatenight                  aggGoodForMeallunch  
    ##                           0.0223419                            0.1190729  
    ##                  aggGoodForMealNone                         review_count  
    ##                           0.0566848                            0.0004052  
    ##                     BikeParkingTRUE                      GoodForKidsTRUE  
    ##                           0.0711981                           -0.0726078  
    ##         RestaurantsReservationsTRUE               RestaurantsTakeOutTRUE  
    ##                           0.0104564                           -0.1221721  
    ##             RestaurantsAttiredressy              RestaurantsAttireformal  
    ##                           0.1103737                            0.2527329

Predicted stars = 3.86 – 0.03(BusinessAcceptsCreditCards) +
0.02(RestaurantsPriceRange2) – 0.09(GoodForKids) + 0.08(BikeParking) –
0.17(Alcoholfull\_bar) – 0.06(Alcoholnone) + 0.04(HasTVTRUE) –
0.18(NoiseLevelloud) + 0.14(NoiseLevelquiet) –
0.43(NoiseLevelvery\_loud) + 0.10(RestaurantsAttiredressy) +
0.21(RestaurantsAttireformal) – 0.10(RestaurantsGoodForGroupsTRUE) +
0.17(CatersTrue) – 0.02(WiFino) – 0.12(WiFipaid) +
0.02(RestaurantsReservationsTRUE) – 0.15(RestaurantsTakeOutTRUE) +
0.01(aggBusinessParkinglot) – 0.13(aggBusinessParkingNone) +
0.14(aggBusinessParkingstreet) – 0.13(aggBusinessParkingNone) +
0.14(aggBusinessParkingstreet) + 0.01(aggBusinessParkingValet) +
0.07(aggBusinessParkingvalidated) + 0.14(aggAmbienceclassy) +
0.11(aggAmbiencedivey) + 0.31(aggAmbiencehipster) +
0.24(aggAmbienceintimate) – 0.12(aggAmbienceNone) +
0.16(aggAmbienceromantic) – 0.20(aggAmbiencetouristy) +
0.21(aggAmbiencetrendy) + 0.18(aggAmbienceupscale) +
0.23(aggGoodForMealbrunch) + 0.11(aggGoodForMealdessert) +
0.20(aggGoodForMealdinner) + 0.02(aggGoodForMeallatenight) +
0.13(aggGoodForMeallunch) + 0.04(aggGoodForMealNone)

This linear model suggests that in an alternate reality, if a restaurant
demonstrated none of the above characteristics, the star rating would be
3.86. For each attribute a restaurant has, its rating increases or
decreases according to each attribute’s coefficient multiplied by the
value assigned to the attribute. The adjusted R-squared value for this
linear model is 0.1584309, meaning that this model accounts for
approximately 15.8% of the variability in the stars values. This low
score indicates that the linear regression model insufficiently
describes the data and has low predictive
power.

``` r
fit <- rpart(stars ~ categories+BusinessAcceptsCreditCards+RestaurantsPriceRange2+GoodForKids+BikeParking+Alcohol+HasTV+NoiseLevel+RestaurantsAttire+RestaurantsGoodForGroups+Caters+WiFi+RestaurantsReservations+RestaurantsTakeOut+aggBusinessParking+aggAmbience+aggGoodForMeal, method = "anova", data = food_reduce)

printcp(fit) # display the results 
```

    ## 
    ## Regression tree:
    ## rpart(formula = stars ~ categories + BusinessAcceptsCreditCards + 
    ##     RestaurantsPriceRange2 + GoodForKids + BikeParking + Alcohol + 
    ##     HasTV + NoiseLevel + RestaurantsAttire + RestaurantsGoodForGroups + 
    ##     Caters + WiFi + RestaurantsReservations + RestaurantsTakeOut + 
    ##     aggBusinessParking + aggAmbience + aggGoodForMeal, data = food_reduce, 
    ##     method = "anova")
    ## 
    ## Variables actually used in tree construction:
    ## [1] aggAmbience    aggGoodForMeal categories    
    ## 
    ## Root node error: 10027/22410 = 0.44742
    ## 
    ## n= 22410 
    ## 
    ##         CP nsplit rel error  xerror      xstd
    ## 1 0.068802      0   1.00000 1.00014 0.0099140
    ## 2 0.025011      1   0.93120 0.93152 0.0088306
    ## 3 0.020776      2   0.90619 0.90783 0.0087151
    ## 4 0.016035      3   0.88541 0.88927 0.0086588
    ## 5 0.013138      4   0.86938 0.87515 0.0085808
    ## 6 0.010000      5   0.85624 0.86406 0.0084920

``` r
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  
```

    ## 
    ## Regression tree:
    ## rpart(formula = stars ~ categories + BusinessAcceptsCreditCards + 
    ##     RestaurantsPriceRange2 + GoodForKids + BikeParking + Alcohol + 
    ##     HasTV + NoiseLevel + RestaurantsAttire + RestaurantsGoodForGroups + 
    ##     Caters + WiFi + RestaurantsReservations + RestaurantsTakeOut + 
    ##     aggBusinessParking + aggAmbience + aggGoodForMeal, data = food_reduce, 
    ##     method = "anova")
    ## 
    ## Variables actually used in tree construction:
    ## [1] aggAmbience    aggGoodForMeal categories    
    ## 
    ## Root node error: 10027/22410 = 0.44742
    ## 
    ## n= 22410 
    ## 
    ##         CP nsplit rel error  xerror      xstd
    ## 1 0.068802      0   1.00000 1.00014 0.0099140
    ## 2 0.025011      1   0.93120 0.93152 0.0088306
    ## 3 0.020776      2   0.90619 0.90783 0.0087151
    ## 4 0.016035      3   0.88541 0.88927 0.0086588
    ## 5 0.013138      4   0.86938 0.87515 0.0085808
    ## 6 0.010000      5   0.85624 0.86406 0.0084920

![](project_files/figure-gfm/rpart%20regression%20tree-1.png)<!-- -->

``` r
prune(fit, cp = 0.02)
```

    ## n= 22410 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ## 1) root 22410 10026.6100 3.538019  
    ##   2) categories=buffets,burgers,fast burgers,fast food,fast mexican,fast sandwiches,restaurants 1553  1128.8920 2.895042  
    ##     4) aggGoodForMeal=breakfast,brunch,None 730   478.7223 2.506164 *
    ##     5) aggGoodForMeal=dessert,dinner,latenight,lunch 823   441.8548 3.239976 *
    ##   3) categories=american (new),american (traditional),asian fusion,bakeries,barbeque,breakfast & brunch,cafes,canadian (new),caribbean,chicken wings,chinese,coffee & tea,delis,desserts,diners,event planning & services,food,french,german,greek,grocery,ice cream & frozen yogurt,indian,italian,japanese,juice bars & smoothies,korean,mediterranean,mexican,middle eastern,nightlife,pizza,salad,sandwiches,seafood,shopping,specialty food,steakhouses,sushi bars,thai,vietnamese,wine & spirits 20857  8207.8700 3.585894  
    ##     6) categories=american (traditional),asian fusion,barbeque,chicken wings,chinese,delis,greek,korean,mexican,nightlife,pizza,steakhouses 12214  4760.2580 3.493655 *
    ##     7) categories=american (new),bakeries,breakfast & brunch,cafes,canadian (new),caribbean,coffee & tea,desserts,diners,event planning & services,food,french,german,grocery,ice cream & frozen yogurt,indian,italian,japanese,juice bars & smoothies,mediterranean,middle eastern,salad,sandwiches,seafood,shopping,specialty food,sushi bars,thai,vietnamese,wine & spirits 8643  3196.8390 3.716244 *

``` r
plot(fit, uniform=TRUE, 
    main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE)
```

![](project_files/figure-gfm/rpart%20regression%20tree-2.png)<!-- -->

Since the R-squared value we got from the multiple linear regression
model was quite low, we wanted to explore results from other potential
regression methods. Our next attempt was to build a regression decision
tree using the rpart package. The decision tree algorithm is better at
capturing non-linearity in the data by dividing space into smaller
sub-spaces. In the following code chunk, we grew the tree using the same
variables as we had in the multiple linear regression model. The
resulting R-squared of the model was 0.14736, meaning that approximately
14.7% of the variability in the star ratings was accounted for by the
model.

Next, we plotted two graph p1 and p2. P1 showed the predicted stars
vs. acutal stars and we colored that data point by restaurant
categories. P2 demonstrated the residuals vs. predicted stars. The
center of the residual plot is approximately in the middle with data
points radiating in a circular shape.

``` r
predicted_stars <- fitted(lm_food)
stars_original <- food_reduce$stars

p1 <-ggplot(lm_food,mapping =aes(stars_original,.fitted, color = factor(categories))) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.5 )+
  labs(x = "Actual stars", y = "Predicted Stars",title = "Predicted vs Actual Stars") +
  theme_minimal() +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))
p1
```

![](project_files/figure-gfm/predicted_residual-1.png)<!-- -->

``` r
residual <- resid(lm_food)
residual_jitter = jitter(residual,amount = .01)
p2 <- ggplot(lm_food,mapping = aes(.fitted,residual_jitter, color = factor(categories))) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.5 )+
  labs(x = "Predicted stars", y = "Residuals",title = "Residual Plot of Stars")+
  theme_minimal() +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")
p2
```

![](project_files/figure-gfm/predicted_residual-2.png)<!-- -->

``` r
# run random forest
fit_full <- randomForest(stars ~  RestaurantsPriceRange2 + categories + BusinessAcceptsCreditCards + Alcohol +  HasTV + NoiseLevel +  RestaurantsGoodForGroups + Caters + WiFi + aggBusinessParking + aggAmbience + aggGoodForMeal + review_count + BikeParking + GoodForKids + RestaurantsReservations + RestaurantsTakeOut + RestaurantsAttire + RestaurantsGoodForGroups, 
                    data=train_full, 
                    importance=TRUE,  
                    ntree=50)

# inspect which elements are important
varImpPlot(fit_full)
```

![](project_files/figure-gfm/random_forest-1.png)<!-- -->

``` r
# get r^2
(r2 <- rSquared(test_full$stars, test_full$stars - predict(fit_full, test_full)))
```

    ##           [,1]
    ## [1,] 0.2872312

``` r
# plot learning curve
plot(fit_full)
```

![](project_files/figure-gfm/random_forest-2.png)<!-- -->

``` r
# get info on model
fit_full
```

    ## 
    ## Call:
    ##  randomForest(formula = stars ~ RestaurantsPriceRange2 + categories +      BusinessAcceptsCreditCards + Alcohol + HasTV + NoiseLevel +      RestaurantsGoodForGroups + Caters + WiFi + aggBusinessParking +      aggAmbience + aggGoodForMeal + review_count + BikeParking +      GoodForKids + RestaurantsReservations + RestaurantsTakeOut +      RestaurantsAttire + RestaurantsGoodForGroups, data = train_full,      importance = TRUE, ntree = 50) 
    ##                Type of random forest: regression
    ##                      Number of trees: 50
    ## No. of variables tried at each split: 6
    ## 
    ##           Mean of squared residuals: 0.3325849
    ##                     % Var explained: 25.79

``` r
# predict values on test set
predict_tbl <- predict(fit_full, test_full) %>% 
  as.data.frame()

# modify predict_tbl
colnames(predict_tbl) <- c("predicted_score")
for (i in 1:nrow(predict_tbl)){
  predict_tbl[i,"index"] <-  i 
}

# find rows with highest predicted stars in test set
predict_tbl %>% 
  top_n(1, predicted_score) %>% 
  pull() %>% 
  test_full[.,] %>% 
  select(-business_id, -address, -postal_code, -latitude, -longitude, -is_open)
```

    ## # A tibble: 1 x 23
    ##   name        neighborhood     city   state stars review_count categories 
    ##   <chr>       <chr>            <chr>  <chr> <dbl>        <int> <fct>      
    ## 1 Café Myria… Plateau-Mont-Ro… Montr… QC       4.           37 coffee & t…
    ## # ... with 16 more variables: BusinessAcceptsCreditCards <lgl>,
    ## #   RestaurantsPriceRange2 <int>, GoodForKids <lgl>, BikeParking <lgl>,
    ## #   Alcohol <fct>, HasTV <lgl>, NoiseLevel <fct>, RestaurantsAttire <fct>,
    ## #   RestaurantsGoodForGroups <lgl>, Caters <lgl>, WiFi <fct>,
    ## #   RestaurantsReservations <lgl>, RestaurantsTakeOut <lgl>,
    ## #   aggBusinessParking <fct>, aggAmbience <fct>, aggGoodForMeal <fct>

Next, we constructed a random forest. The random forest algorithm is an
ensemble learning method that can be used for classification, regression
and other tasks. By training a multitude of decision trees, random
forest is optimized to have better predicted power. It discovers more
complex dependencies at the cost of more time needed to fit a model.
Compared to linear regression, random forest outperforms when there are
nonlinear dependencies. The resulting R-squared of the model was 0.277,
meaning that approximately 27.7% of the variability in the star ratings
was accounted for by the model. In addition we got RMSE of 0.333, giving
a standard deviation of 0.57. From this we can construct a 95%
confidence interval around our predictions. We are 95% confident that
the actual star rating will be between ±1.14 stars of the predicted
value.

``` r
# this function performs the above analysis for any subset of the dataset
random_forest <- function(x){
  food_reduce <- food_reduce %>% 
    filter(categories == x)

  cols = c("Alcohol", "NoiseLevel", "RestaurantsAttire", "WiFi", "aggBusinessParking", "aggAmbience", "aggGoodForMeal", "categories")
  food_reduce[cols] <- lapply(food_reduce[cols], factor)
  
  train_full <- food_reduce %>% 
    head(9*nrow(food_reduce)/10)
  
  test_full <- food_reduce %>% 
    tail(1*nrow(food_reduce)/10)
  
  fit_full <- randomForest(stars ~  RestaurantsPriceRange2 + categories + BusinessAcceptsCreditCards + Alcohol +  HasTV + NoiseLevel +  RestaurantsGoodForGroups + Caters + WiFi + aggBusinessParking + aggAmbience + aggGoodForMeal + review_count + BikeParking + GoodForKids + RestaurantsReservations + RestaurantsTakeOut + RestaurantsAttire + RestaurantsGoodForGroups , 
                      data=train_full, 
                      importance=TRUE,  
                      ntree=200)
  
  r2 <- round(rSquared(test_full$stars, test_full$stars - predict(fit_full, test_full)),3) %>% 
    as.data.frame()
  
  colnames(r2) <- "r.squared" 
    
  predict_tbl <- predict(fit_full, test_full) %>% 
  data.frame()

  colnames(predict_tbl) <- c("predicted_score")
  for (i in 1:nrow(predict_tbl)){
    predict_tbl[i,"index"] <-  i 
  }

  # find rows with highest predicted stars in test set
  predict_tbl <- predict_tbl %>% 
    top_n(3, predicted_score) 
  
  output <- predict_tbl %>% 
    pull(index) %>% 
    test_full[.,] %>% 
    select(-business_id, -address, -postal_code, -latitude, -longitude, -is_open) 

  # plot(fit_full, main = x)
  
  return(cbind(predict_tbl[1], r2, output))
  #return(as.data.frame(r2))

}

#Apply the function random forest to each element of the vector large_categories_list
categories_rows <-large_categories_list[1:20]

r_squared <- purrr::map(categories_rows,random_forest)
```

    ## Warning in randomForest.default(m, y, ...): The response has five or fewer
    ## unique values. Are you sure you want to do regression?
    
    ## Warning in randomForest.default(m, y, ...): The response has five or fewer
    ## unique values. Are you sure you want to do regression?

``` r
r_categories <-cbind(r_squared,categories_rows)
r_categories_dt <- as.data.frame(r_categories, stringsAsFactors = FALSE)

best_row <- purrr::map_df(categories_rows,random_forest)
```

    ## Warning in randomForest.default(m, y, ...): The response has five or fewer
    ## unique values. Are you sure you want to do regression?
    
    ## Warning in randomForest.default(m, y, ...): The response has five or fewer
    ## unique values. Are you sure you want to do regression?

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

``` r
best_row %>% 
  arrange(desc(r.squared), desc(predicted_score)) %>% 
  select(categories, everything())
```

    ##                   categories predicted_score r.squared
    ## 1                  fast food        3.812142     0.367
    ## 2                  fast food        3.732741     0.367
    ## 3                  fast food        3.706304     0.367
    ## 4                  nightlife        4.170078     0.271
    ## 5                  nightlife        4.167543     0.271
    ## 6                  nightlife        4.163434     0.271
    ## 7                    burgers        3.987809     0.271
    ## 8                    burgers        3.986583     0.271
    ## 9                    burgers        3.840383     0.271
    ## 10                   mexican        4.076791     0.233
    ## 11                   mexican        4.027408     0.233
    ## 12                   mexican        3.997686     0.233
    ## 13                     pizza        4.294224     0.224
    ## 14                     pizza        4.279203     0.224
    ## 15                     pizza        4.199095     0.224
    ## 16 ice cream & frozen yogurt        3.991812     0.206
    ## 17 ice cream & frozen yogurt        3.666673     0.206
    ## 18 ice cream & frozen yogurt        3.645208     0.206
    ## 19    american (traditional)        4.328486     0.200
    ## 20                   grocery        4.315000     0.200
    ## 21    american (traditional)        4.243765     0.200
    ## 22    american (traditional)        4.171506     0.200
    ## 23                   grocery        4.114563     0.200
    ## 24                   grocery        4.007833     0.200
    ## 25             mediterranean        4.158783     0.156
    ## 26             mediterranean        4.153620     0.156
    ## 27             mediterranean        4.148619     0.156
    ## 28            american (new)        4.143676     0.151
    ## 29            american (new)        4.102104     0.151
    ## 30            american (new)        4.027821     0.151
    ## 31        breakfast & brunch        4.187622     0.141
    ## 32        breakfast & brunch        4.085118     0.141
    ## 33        breakfast & brunch        4.002803     0.141
    ## 34            specialty food        4.257487     0.132
    ## 35            specialty food        4.255472     0.132
    ## 36            specialty food        4.139917     0.132
    ## 37                sandwiches        4.194335     0.129
    ## 38                sandwiches        4.190177     0.129
    ## 39                sandwiches        4.156233     0.129
    ## 40              coffee & tea        4.473542     0.128
    ## 41              coffee & tea        4.340438     0.128
    ## 42              coffee & tea        4.309815     0.128
    ## 43                    indian        4.050969     0.108
    ## 44                    indian        4.044452     0.108
    ## 45                    indian        4.023720     0.108
    ## 46                   italian        4.215688     0.107
    ## 47                   italian        4.191173     0.107
    ## 48                   italian        4.094316     0.107
    ## 49                  japanese        4.191841     0.070
    ## 50                  japanese        4.135562     0.070
    ## 51                  japanese        4.055952     0.070
    ## 52                   chinese        3.951260     0.064
    ## 53                   chinese        3.907758     0.064
    ## 54                   chinese        3.872973     0.064
    ## 55                  bakeries        4.363485    -0.046
    ## 56                  bakeries        4.238026    -0.046
    ## 57                  bakeries        4.232235    -0.046
    ## 58                  shopping        3.898625      -Inf
    ##                                                name
    ## 1                                          Hot-Star
    ## 2                                       Chick-fil-A
    ## 3                         Flock Rotisserie + Greens
    ## 4                               Hakkasan Restaurant
    ## 5                           Yours Truly Restaurants
    ## 6                                   The Refuge Cafe
    ## 7                                         The Stand
    ## 8                             Aioli Gourmet Burgers
    ## 9                                         Five Guys
    ## 10                                    Backyard Taco
    ## 11                  Cuban Foods Bakery & Restaurant
    ## 12                          Escobar Mexican Kitchen
    ## 13                                       The Parlor
    ## 14                          Blaze Fast-Fire'd Pizza
    ## 15                                 Joe's Farm Grill
    ## 16                                 Nekter Juice Bar
    ## 17                                   Baskin Robbins
    ## 18                                         Culver's
    ## 19               Rise & Shine - A Steak & Egg Place
    ## 20 Yusef's Middle Eastern Restaurant Grocery & Deli
    ## 21                                      Schlotzky's
    ## 22                                         Cafe Rio
    ## 23                      Rainbow's End Natural Foods
    ## 24                                       Cedar Land
    ## 25                                     Pitta Souvli
    ## 26                                   Phoenicia Cafe
    ## 27                       Olives Mediterranean Grill
    ## 28                                           O.Noir
    ## 29                                       AGO Bistro
    ## 30                                      Chick-fil-A
    ## 31       Daily Kitchen Modern Eatery and Rotisserie
    ## 32                                        Chompie's
    ## 33                                 Pastries N Chaat
    ## 34                         Nana's Soul Food Kitchen
    ## 35                                    The Rice Shop
    ## 36                                    Soulfish Poke
    ## 37                               Gourmet Quick Bite
    ## 38                                  Pho Thanh Huong
    ## 39                                       Let Me Eat
    ## 40                                  Café Myriade II
    ## 41                                       Bradbury's
    ## 42                                  The Coffee Shop
    ## 43                               Mint Indian Bistro
    ## 44                    Mount Everest India's Cuisine
    ## 45                                 Passage To India
    ## 46                                    Gina's Bistro
    ## 47                                Papa John's Pizza
    ## 48                                      Arrivederci
    ## 49                 Sushi Hiroyoshi Japanese Cuisine
    ## 50                         Gangnam Asian BBQ Dining
    ## 51                            Blue Fin Sushi & Roll
    ## 52                                New Asian Kitchen
    ## 53                                   Bp Street Cafe
    ## 54                                     Super Dragon
    ## 55                                  Presti's Bakery
    ## 56                         European Bakery and Cafe
    ## 57                                           Humble
    ## 58               Queen Creek Olive Mill Marketplace
    ##                neighborhood         city state stars review_count
    ## 1             Downtown Core      Toronto    ON   3.5          218
    ## 2                      <NA>      Phoenix    AZ   4.0           96
    ## 3    Entertainment District      Toronto    ON   4.0           97
    ## 4                 The Strip    Las Vegas    NV   4.0          372
    ## 5                      <NA>       Mentor    OH   3.5           40
    ## 6                      <NA>      Phoenix    AZ   4.5          173
    ## 7                      <NA>      Phoenix    AZ   4.0          784
    ## 8                      <NA>      Phoenix    AZ   4.0          135
    ## 9                  Eastside    Las Vegas    NV   4.0          109
    ## 10                     <NA>         Mesa    AZ   4.5          892
    ## 11                     <NA>      Phoenix    AZ   4.5          200
    ## 12                     <NA>      Phoenix    AZ   4.5          132
    ## 13                     <NA>      Phoenix    AZ   4.0         1081
    ## 14                   Anthem    Henderson    NV   4.5          400
    ## 15                     <NA>      Gilbert    AZ   4.0         1644
    ## 16                     <NA>      Phoenix    AZ   4.0           61
    ## 17                 Eastside    Las Vegas    NV   4.0           32
    ## 18                     <NA> Cross Plains    WI   3.5           14
    ## 19                Southwest    Las Vegas    NV   4.0         1191
    ## 20                     <NA>      Phoenix    AZ   4.0           44
    ## 21                     <NA>    Henderson    NV   3.5           37
    ## 22                     <NA>    Henderson    NV   3.5          261
    ## 23                 Downtown    Las Vegas    NV   4.0           94
    ## 24                 Eastland    Charlotte    NC   4.0           34
    ## 25                     <NA>     Chandler    AZ   4.5          260
    ## 26                     <NA>        Tempe    AZ   4.0          191
    ## 27                     <NA>      Phoenix    AZ   4.5           65
    ## 28 Church-Wellesley Village      Toronto    ON   3.5          273
    ## 29            Downtown Core      Toronto    ON   3.5           87
    ## 30                     <NA>      Phoenix    AZ   4.0          121
    ## 31          South Summerlin    Las Vegas    NV   3.5          195
    ## 32                     <NA>   Scottsdale    AZ   3.5          418
    ## 33                     <NA>     Chandler    AZ   3.5           94
    ## 34                     <NA>    Charlotte    NC   4.0          274
    ## 35            Spring Valley    Las Vegas    NV   4.5          128
    ## 36                     <NA>    Las Vegas    NV   3.5          174
    ## 37                Southeast    Las Vegas    NV   4.5           35
    ## 38                Southeast    Las Vegas    NV   4.0          403
    ## 39                 Old Town    Edinburgh   EDH   4.5           12
    ## 40       Plateau-Mont-Royal     Montréal    QC   4.0           37
    ## 41                  Capitol      Madison    WI   4.5          293
    ## 42                     <NA>      Gilbert    AZ   4.0          490
    ## 43                 Eastside    Las Vegas    NV   4.0          944
    ## 44                 Westside    Las Vegas    NV   4.5         1152
    ## 45                     <NA>        Tempe    AZ   3.5          185
    ## 46            Spring Valley    Las Vegas    NV   4.5          187
    ## 47                     <NA>         Mesa    AZ   2.5            8
    ## 48                     <NA>   Scottsdale    AZ   4.0          219
    ## 49                 Westside    Las Vegas    NV   5.0          177
    ## 50                 Eastside    Las Vegas    NV   4.5         3262
    ## 51                     <NA>    Las Vegas    NV   4.0          436
    ## 52                     <NA>      Phoenix    AZ   4.0          123
    ## 53                     <NA>        Tempe    AZ   4.0          105
    ## 54                     <NA>      Phoenix    AZ   4.0          144
    ## 55             Little Italy    Cleveland    OH   4.0          285
    ## 56                     <NA>     Glendale    AZ   4.0           96
    ## 57                   Regent      Madison    WI   4.5           49
    ## 58                     <NA>      Phoenix    AZ   5.0            7
    ##    BusinessAcceptsCreditCards RestaurantsPriceRange2 GoodForKids
    ## 1                        TRUE                      2        TRUE
    ## 2                        TRUE                      1        TRUE
    ## 3                        TRUE                      2        TRUE
    ## 4                        TRUE                      4       FALSE
    ## 5                        TRUE                      2        TRUE
    ## 6                        TRUE                      1        TRUE
    ## 7                        TRUE                      1        TRUE
    ## 8                        TRUE                      2        TRUE
    ## 9                        TRUE                      1        TRUE
    ## 10                       TRUE                      1        TRUE
    ## 11                       TRUE                      1        TRUE
    ## 12                       TRUE                      1        TRUE
    ## 13                       TRUE                      2        TRUE
    ## 14                       TRUE                      1        TRUE
    ## 15                       TRUE                      2        TRUE
    ## 16                       TRUE                      2        TRUE
    ## 17                       TRUE                      1        TRUE
    ## 18                       TRUE                      1        TRUE
    ## 19                       TRUE                      2        TRUE
    ## 20                       TRUE                      1        TRUE
    ## 21                       TRUE                      1        TRUE
    ## 22                       TRUE                      1        TRUE
    ## 23                       TRUE                      2        TRUE
    ## 24                       TRUE                      1        TRUE
    ## 25                       TRUE                      2        TRUE
    ## 26                       TRUE                      1        TRUE
    ## 27                       TRUE                      1        TRUE
    ## 28                       TRUE                      3       FALSE
    ## 29                       TRUE                      3       FALSE
    ## 30                       TRUE                      1        TRUE
    ## 31                       TRUE                      2        TRUE
    ## 32                       TRUE                      2        TRUE
    ## 33                       TRUE                      1        TRUE
    ## 34                       TRUE                      2        TRUE
    ## 35                       TRUE                      1        TRUE
    ## 36                       TRUE                      2        TRUE
    ## 37                       TRUE                      1        TRUE
    ## 38                       TRUE                      1        TRUE
    ## 39                      FALSE                      1        TRUE
    ## 40                      FALSE                      1       FALSE
    ## 41                       TRUE                      1        TRUE
    ## 42                       TRUE                      1        TRUE
    ## 43                       TRUE                      2        TRUE
    ## 44                       TRUE                      2        TRUE
    ## 45                       TRUE                      2        TRUE
    ## 46                       TRUE                      2        TRUE
    ## 47                       TRUE                      2        TRUE
    ## 48                       TRUE                      2       FALSE
    ## 49                       TRUE                      3        TRUE
    ## 50                       TRUE                      2        TRUE
    ## 51                       TRUE                      2        TRUE
    ## 52                       TRUE                      1        TRUE
    ## 53                       TRUE                      1        TRUE
    ## 54                       TRUE                      2        TRUE
    ## 55                       TRUE                      1        TRUE
    ## 56                       TRUE                      1        TRUE
    ## 57                       TRUE                      1        TRUE
    ## 58                       TRUE                      2       FALSE
    ##    BikeParking       Alcohol HasTV NoiseLevel RestaurantsAttire
    ## 1         TRUE          none FALSE    average            casual
    ## 2         TRUE          none FALSE    average            casual
    ## 3        FALSE          none FALSE    average            casual
    ## 4        FALSE      full_bar FALSE    average            dressy
    ## 5         TRUE beer_and_wine  TRUE    average            casual
    ## 6         TRUE beer_and_wine  TRUE      quiet            casual
    ## 7         TRUE          none FALSE    average            casual
    ## 8         TRUE          none  TRUE    average            casual
    ## 9         TRUE          none FALSE    average            casual
    ## 10        TRUE          none FALSE    average            casual
    ## 11        TRUE          none FALSE    average            casual
    ## 12       FALSE      full_bar  TRUE    average            casual
    ## 13        TRUE      full_bar  TRUE    average            casual
    ## 14        TRUE beer_and_wine FALSE    average            casual
    ## 15        TRUE          none FALSE    average            casual
    ## 16        TRUE          none FALSE      quiet            casual
    ## 17        TRUE          none FALSE      quiet            casual
    ## 18        TRUE          none  TRUE    average            casual
    ## 19        TRUE          none FALSE    average            casual
    ## 20        TRUE          none FALSE      quiet            casual
    ## 21        TRUE          none FALSE    average            casual
    ## 22        TRUE          none FALSE    average            casual
    ## 23        TRUE          none FALSE      quiet            casual
    ## 24        TRUE          none  TRUE      quiet            casual
    ## 25        TRUE beer_and_wine FALSE    average            casual
    ## 26        TRUE          none  TRUE    average            casual
    ## 27        TRUE beer_and_wine FALSE      quiet            casual
    ## 28        TRUE      full_bar FALSE    average            casual
    ## 29        TRUE      full_bar FALSE    average            dressy
    ## 30        TRUE          none FALSE    average            casual
    ## 31        TRUE          none FALSE      quiet            casual
    ## 32        TRUE beer_and_wine  TRUE    average            casual
    ## 33        TRUE          none FALSE    average            casual
    ## 34        TRUE          none  TRUE    average            casual
    ## 35        TRUE          none  TRUE      quiet            casual
    ## 36        TRUE          none FALSE      quiet            casual
    ## 37        TRUE          none  TRUE      quiet            casual
    ## 38       FALSE          none  TRUE    average            casual
    ## 39        TRUE          none FALSE    average            casual
    ## 40        TRUE          none FALSE      quiet            casual
    ## 41        TRUE beer_and_wine FALSE    average            casual
    ## 42        TRUE          none FALSE    average            casual
    ## 43        TRUE      full_bar FALSE    average            casual
    ## 44        TRUE beer_and_wine  TRUE    average            casual
    ## 45        TRUE      full_bar  TRUE      quiet            casual
    ## 46        TRUE beer_and_wine FALSE      quiet            casual
    ## 47        TRUE          none FALSE    average            casual
    ## 48        TRUE      full_bar FALSE      quiet            casual
    ## 49        TRUE beer_and_wine FALSE      quiet            casual
    ## 50        TRUE beer_and_wine  TRUE    average            casual
    ## 51        TRUE beer_and_wine  TRUE    average            casual
    ## 52        TRUE          none  TRUE      quiet            casual
    ## 53        TRUE          none  TRUE      quiet            casual
    ## 54        TRUE beer_and_wine FALSE      quiet            casual
    ## 55        TRUE          none FALSE    average            casual
    ## 56        TRUE          none FALSE      quiet            casual
    ## 57       FALSE          none FALSE      quiet            casual
    ## 58        TRUE beer_and_wine FALSE      quiet            casual
    ##    RestaurantsGoodForGroups Caters WiFi RestaurantsReservations
    ## 1                     FALSE  FALSE   no                   FALSE
    ## 2                      TRUE   TRUE free                   FALSE
    ## 3                     FALSE   TRUE free                   FALSE
    ## 4                      TRUE  FALSE free                    TRUE
    ## 5                      TRUE   TRUE free                   FALSE
    ## 6                      TRUE  FALSE free                   FALSE
    ## 7                      TRUE  FALSE   no                   FALSE
    ## 8                      TRUE   TRUE free                    TRUE
    ## 9                      TRUE  FALSE   no                   FALSE
    ## 10                     TRUE   TRUE   no                   FALSE
    ## 11                    FALSE   TRUE free                   FALSE
    ## 12                     TRUE   TRUE   no                    TRUE
    ## 13                     TRUE   TRUE free                   FALSE
    ## 14                     TRUE  FALSE free                   FALSE
    ## 15                     TRUE  FALSE free                   FALSE
    ## 16                     TRUE  FALSE free                   FALSE
    ## 17                     TRUE   TRUE   no                   FALSE
    ## 18                     TRUE  FALSE free                   FALSE
    ## 19                     TRUE   TRUE   no                   FALSE
    ## 20                    FALSE   TRUE   no                   FALSE
    ## 21                     TRUE   TRUE   no                   FALSE
    ## 22                     TRUE   TRUE   no                   FALSE
    ## 23                     TRUE  FALSE   no                    TRUE
    ## 24                     TRUE   TRUE   no                    TRUE
    ## 25                     TRUE   TRUE free                    TRUE
    ## 26                     TRUE   TRUE free                   FALSE
    ## 27                     TRUE   TRUE free                   FALSE
    ## 28                     TRUE  FALSE   no                    TRUE
    ## 29                     TRUE  FALSE free                    TRUE
    ## 30                     TRUE   TRUE free                   FALSE
    ## 31                     TRUE   TRUE free                   FALSE
    ## 32                     TRUE   TRUE   no                    TRUE
    ## 33                     TRUE   TRUE free                    TRUE
    ## 34                     TRUE   TRUE   no                   FALSE
    ## 35                    FALSE   TRUE   no                   FALSE
    ## 36                    FALSE  FALSE   no                   FALSE
    ## 37                     TRUE   TRUE free                   FALSE
    ## 38                     TRUE   TRUE free                    TRUE
    ## 39                    FALSE   TRUE   no                   FALSE
    ## 40                    FALSE  FALSE   no                   FALSE
    ## 41                    FALSE  FALSE free                   FALSE
    ## 42                     TRUE  FALSE free                   FALSE
    ## 43                     TRUE   TRUE   no                    TRUE
    ## 44                     TRUE   TRUE   no                    TRUE
    ## 45                     TRUE   TRUE   no                    TRUE
    ## 46                     TRUE   TRUE   no                    TRUE
    ## 47                    FALSE   TRUE   no                   FALSE
    ## 48                     TRUE   TRUE   no                    TRUE
    ## 49                     TRUE  FALSE   no                    TRUE
    ## 50                     TRUE   TRUE free                    TRUE
    ## 51                     TRUE  FALSE   no                    TRUE
    ## 52                     TRUE   TRUE   no                   FALSE
    ## 53                     TRUE   TRUE free                   FALSE
    ## 54                     TRUE   TRUE   no                    TRUE
    ## 55                     TRUE   TRUE free                   FALSE
    ## 56                    FALSE   TRUE   no                   FALSE
    ## 57                    FALSE   TRUE free                   FALSE
    ## 58                     TRUE  FALSE free                   FALSE
    ##    RestaurantsTakeOut aggBusinessParking aggAmbience aggGoodForMeal
    ## 1                TRUE             street      casual          lunch
    ## 2                TRUE                lot      casual      breakfast
    ## 3                TRUE             street      casual          lunch
    ## 4               FALSE              valet     upscale         dinner
    ## 5                TRUE                lot      casual         brunch
    ## 6                TRUE                lot      casual         brunch
    ## 7                TRUE                lot      casual         dinner
    ## 8                TRUE                lot      casual         dinner
    ## 9                TRUE                lot      casual         dinner
    ## 10               TRUE                lot      casual         dinner
    ## 11               TRUE               None      casual        dessert
    ## 12               TRUE                lot      casual         dinner
    ## 13               TRUE                lot      trendy         dinner
    ## 14               TRUE                lot      casual         dinner
    ## 15               TRUE                lot      casual         dinner
    ## 16               TRUE               None     hipster      breakfast
    ## 17               TRUE             street        None        dessert
    ## 18               TRUE                lot      casual        dessert
    ## 19               TRUE                lot      casual         brunch
    ## 20               TRUE                lot      casual          lunch
    ## 21               TRUE             street      casual          lunch
    ## 22               TRUE                lot      casual         dinner
    ## 23              FALSE             street        None          lunch
    ## 24               TRUE                lot      casual         brunch
    ## 25               TRUE                lot      casual         dinner
    ## 26               TRUE                lot      casual         dinner
    ## 27               TRUE                lot      casual         dinner
    ## 28              FALSE             street        None         dinner
    ## 29              FALSE             street        None         dinner
    ## 30               TRUE                lot      casual          lunch
    ## 31               TRUE                lot      casual         brunch
    ## 32               TRUE                lot      casual         brunch
    ## 33               TRUE             street    intimate        dessert
    ## 34               TRUE                lot      casual         dinner
    ## 35               TRUE                lot      casual          lunch
    ## 36               TRUE                lot      casual         brunch
    ## 37               TRUE               None      casual         brunch
    ## 38               TRUE                lot      casual         dinner
    ## 39               TRUE               None      casual          lunch
    ## 40               TRUE             street     hipster           None
    ## 41               TRUE             street     hipster         brunch
    ## 42               TRUE                lot      casual        dessert
    ## 43               TRUE                lot      casual         dinner
    ## 44               TRUE                lot      casual         dinner
    ## 45               TRUE                lot      casual         dinner
    ## 46               TRUE                lot    intimate         dinner
    ## 47               TRUE                lot        None           None
    ## 48               TRUE                lot    intimate         dinner
    ## 49               TRUE                lot    intimate         dinner
    ## 50               TRUE                lot      trendy        dessert
    ## 51               TRUE                lot      casual         dinner
    ## 52               TRUE                lot      casual         dinner
    ## 53               TRUE                lot      casual         dinner
    ## 54               TRUE                lot      casual         dinner
    ## 55               TRUE             street      casual        dessert
    ## 56               TRUE                lot      casual        dessert
    ## 57               TRUE             street      casual        dessert
    ## 58              FALSE          validated      casual           None

Finally, we used this random forest analysis to look at subsets of our
dataset. We looked at the most frequent categories and then one by one
filtered the dataset by one of them. We did a random forest analysis for
each one, getting R-squared values as well as the top row in that
dataset with the highest predicted star rating. Interestingly,
fast\_food was the category whose star values were best predicted by the
data — it had an R-squared value of 38%\!
