
# FIGURE SHOWING A HEAT MAP OF ACTIVITIES
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(ggspatial)

activity_locations <- activity_types %>% 
  select(algorithm) %>% 
  unnest(cols = c(algorithm)) %>% 
  st_as_sf()
  
activities <- ggplot(data = activity_locations) + 
  annotation_map_tile("cartolight") +
  geom_contour()

# Compute density of points
density <- density(activity_locations$geometry)

# Plot the sf object with density
ggplot() +
  geom_sf(data = activity_locations, aes(fill = density)) +
  scale_fill_viridis_c() +  # Choose your desired color palette
  theme_void()  # Remove axes and background


# Load required libraries
library(sf)
library(ggplot2)
library(viridis)
library(osmdata)

## PARKS
# Load the GeoJSON shape file
tar_load(parksSf)
parks <- parksSf

# Calculate the bounding box of the parks sf object
parks_bbox <- st_bbox(parks)

# Plot the shape files and overlay the activity locations, zoomed to the parks extent
ggplot() +
  geom_sf(data = activity_locations, color = "red", size = 2, alpha = 0.3) + # Activity locations
  geom_sf(data = parks, fill = "black", color = "white") + # Base map
  scale_color_viridis_c() + 
  theme_minimal() + 
  coord_sf(xlim = c(parks_bbox["xmin"], parks_bbox["xmax"]),
           ylim = c(parks_bbox["ymin"], parks_bbox["ymax"])) # Set limits based on parks bounding box


## UNIQUE USERIDS AT DIFFERENT POINTS
# 65
tar_load(activity_types)
acts <- activity_types %>% group_by(userId) %>% n_groups
acts

# 89
tar_load(demo_ids)
dem <- demo_ids %>% group_by(userId) %>%  n_groups
dem

# 88
tar_load(survey_data)
sur <- survey_data %>% group_by(userId) %>%  n_groups
sur

#88
tar_load(imputed_trips)
imp <- imputed_trips %>%  group_by(userId) %>% n_groups()
imp

#88
tar_load(final_table)
fin <- final_table %>%  group_by(userId) %>% n_groups
fin












