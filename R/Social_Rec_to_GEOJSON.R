# Load packages
library(osmdata)
library(ggspatial)
library(sf)
library(tidyverse)

# Define bbox for Utah County
bbox <- getbb("Utah County, Utah")

### RESTAURANTS ###
# Define types of sustenance locations
sustenance <- c('restaurant', 'cafe', 'fast_food','food_court', 'ice_cream')

# Get restaurant locations in Utah County
get_restaurants <- opq(bbox) %>%
  add_osm_feature(key = 'amenity', value = sustenance) %>%
  osmdata_sf() # Get a list of sf data frames for these tags

# Extract restaurant polygons
restaurants_polygons <- pluck(get_restaurants, "osm_polygons")

### ENTERTAINMENT ###
# Define types of entertainment locations
entertainment <- c('cinema', 'community_centre', 'theatre')

# Get entertainment locations in Utah County
get_entertainment <- opq(bbox) %>%
  add_osm_feature(key = 'amenity', value = entertainment) %>%
  osmdata_sf() # Get a list of sf data frames for these tags

# Extract entertainment polygons
entertainment_polygons <- pluck(get_entertainment, "osm_polygons")

### SHOPPING ###
# Define types of shopping locations
shopping <- c('department_store', 'mall')

# Get shopping locations in Utah County
get_shopping <- opq(bbox) %>%
  add_osm_feature(key = 'shop', value = shopping) %>%
  osmdata_sf() # Get a list of sf data frames for these tags

# Extract shopping polygons
shopping_polygons <- pluck(get_shopping, "osm_polygons")

# Combine all polygons
all_polygons <- bind_rows(restaurants_polygons, entertainment_polygons, shopping_polygons)

# Write polygons to GeoJSON file
st_write(all_polygons, "social_rec.geojson")
