PROJECT TITLE
================
kimchi-stew
March 25th, 2018

Section 1. Introduction
-----------------------

What distinguishes an excellent restaurant from others. This dataset is from Kaggle, collected using the Zomato API. Zomato is a platform where people can contribute ratings of restaurants around the world.
The variables are restuarant ID, resturant name, country code, city, address, locality, locality verbose, longitude, latitude, cuisines, average cost for two, currency, has table booking, has online delivery, is delivering now, switch to order, price range, aggregate rating, rating color, rating text, votes.

Section 2. Data analysis plan
-----------------------------

The outcome is the rating text (categorical variable), and the predictors are cuisines, average cost for two(which we need to convert the currency to usd later), has online delivery, location (longitude and latitude).

Statistical methods: Multiple linear regression + Model Selection Hypothesis testing Plotting/mapping of data points

TODO: preliminary data analysis

Hypothesis: there will be two types of "Excellent" restaurants, the first being affordable and the other being expensive and high-end. Additionally, restaurants with a larger number of votes will have a higher aggregate rating.

To support our hypothesis: We will need linear models with high R-squared values, hypothesis tests with p values below the significance level, and the map will show if regional patterns in rating exist as well as how the definition of an excellent restaurant varies geographically.

Section 3. Data
---------------

TO DO: add dimensions and codebook to the README in this folder. Then print out the output of glimpse of your data frame.

``` r
library(tidyverse)
```

    ## Warning: running command 'timedatectl' had status 1

    ## ── Attaching packages ──────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.1     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.7.2     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ─────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
