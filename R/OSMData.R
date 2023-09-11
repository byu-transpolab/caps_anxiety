# This script creates a map of restaurants/food locations in Utah County

# load packages
library(osmdata)
library(ggspatial)
library(sf)
library(tidyverse)


# Function for finding restaurants within Utah County
restaurants <- function(county_name) {
  bbox <- c(39.9, -112.1, -111.5, 40.48)
  
  sustenance <- c('restaurant', 'cafe', 'fast_food', 'ice_cream')
  get_restaurants <- opq(bbox) %>% #
    add_osm_feature(key = 'amenity', value = sustenance) %>%
    osmdata_sf()
  restaurant_points <- bind_rows(
    pluck(get_restaurants, "osm_points"),
    st_centroid(pluck(get_restaurants, "osm_polygons"))
  )

  restaurants_geojson <- geojson_json(restaurant_points)
  
  return(restaurants_geojson)
}

# Still encountered the "HTTP 405 Method Not Allowed" Error, this is another attempt
getRestaurants <- function(county_name) {
  # Query for restaurant-related amenities within the specified bounding box
  sustenance <- c('restaurant', 'cafe', 'fast_food', 'ice_cream')
  osm_query <- opq(bbox = county_name) %>%
    add_osm_feature(key = 'amenity', value = sustenance)
  
  # Fetch and retrieve the data
  osm_data <- osmdata_sf(osm_query)
  
  # Extract restaurant points
  restaurant_points <- osm_data$osm_points
  
  return(restaurant_points)
}

# For Utah County
county_name <- "Utah County, Utah"
restaurants_data <- getRestaurants(county_name)



# display restaurant points on a map
ggplot() +
  # add base map
  annotation_map_tile(type = 'cartolight', zoomin = 0) +

  # add Utah County boundary
  #geom_sf(data = bbox, colour = "grey40", fill = NA, linewidth = 1.5) +

  # add restaurants
  geom_sf(data = restaurants_points, colour = "darkred") +

  # create labels
  labs(captions = "contains data from OpenStreetMap")+
  theme_void()
