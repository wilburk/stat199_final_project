
library(shiny)
library(tidyverse)
library(leaflet)
load("data/food.cvs")

map <- leaflet(width = 400, height = 400)
map <- addTiles(map)
map <- setView(map, lng = -123.251,
               lat = 49.263,
               zoom = 6)
