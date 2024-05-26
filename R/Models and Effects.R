
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




# [say i am using this methodology because this methodology will allow me to focus on these key things...] 
# 
# [why this methodology.... explain why this will let me narrow in]
# 
# [justify the methods, narrow down what i am doing and why i am doing it]
# 
# Following the integration of semantic, or interpreted, activities and survey responses, statistical models were constructed to examine the interplay between mental health and travel behavior. Three base models were formulated: ordinary least squares, fixed effects, and random effects models. These models predict individual well-being based on a variable representing travel behavior, with the fixed effects and random effects models accounting for individual baseline differences in well-being. Moreover, data transformations were applied to further understand the impact of the number of activities and distance traveled on reported motivation levels.


# In terms of activities at specific locations, engagement at parks, grocery stores, libraries, and social recreation locations was generally infrequent across all groups. For instance, activities at parks yielded minimal mean values and low standard deviations, suggesting sporadic engagement. Similarly, activities at grocery stores and libraries were rare, with mean values close to zero and minimal variability. Notably, individuals with social anxiety exhibited slightly higher mean values and greater variability in activities at grocery stores compared to other groups. Activities at social recreation locations, while still infrequent, showed slightly higher mean values across all groups, with some variability in engagement, particularly among individuals with control and social anxiety.


# Moreover, when examining the number of unique participants in each group included in the FE models, the numbers decrease further. Only 5 participants remain in the control group, 8 in the autism group, and 19 in the social anxiety group. 


library(purrr)


ms <- mtcars |>
  group_by(am) |>
  nest() |>
  mutate(
    model = map(data, function(d) lm(mpg ~ disp, data = d))
  ) 


modelsummary(ms$model |> set_names(str_c("AM: ", ms$am)))

