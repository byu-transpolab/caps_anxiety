
library(sf)
library(geometry)
library(tidyverse)

tar_load(preprocessed)
tar_load(scored_days)

new_paras <- preprocessed %>% 
  arrange(userId, activityDay, hour, minute) %>% 
  group_by(userId, activityDay, hour, minute) %>%
  slice_sample(n = 6, replace = FALSE) %>%
  ungroup() %>% 
  group_by(userId, activityDay) %>%
  nest() %>% 
  ungroup() %>%
  rename(cleaned = data)


# Extract the nested tibble
nested_tibble <- new_paras$cleaned[[1]]

# Extract latitude and longitude columns
coordinates <- nested_tibble[, c("lon", "lat")]

# Compute convex hull
convex_hull <- chull(coordinates)

# You can use these indices to retrieve the actual points if needed
convex_hull_points <- coordinates[convex_hull, ]

convex_hull_points <- convex_hull_points 
  
# Print the convex hull points
print(convex_hull_points)

# Plot the original points
plot(coordinates, pch = 20, col = "blue", main = "Convex Hull Plot")

# Plot the convex hull points in a different color
points(convex_hull_points, pch = 20, col = "darkgreen")

# Connect all convex hull points to form a polygon
polygon(convex_hull_points, border = "red")








makeSf <- function(df, crs = 32612) {
  df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs)
}

filtered_user <- scored_days %>% 
  filter(userId == "5dd82431461a03631bbeb127")

processed_days_unnest <- filtered_user %>%
  arrange(userId, activityDay, hour, minute) %>% 
  group_by(userId, activityDay, hour, minute) %>%
  slice_sample(n = 6, replace = FALSE) %>%
  ungroup() %>% 
  group_by(userId, activityDay) %>%
  nest() %>% 
  ungroup() %>%
  rename(cleaned = data) %>%
  mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
  filter(num_points > 1000) %>% 
  # Convert the LBS points to an SF object
  mutate(cleaned = purrr::map(cleaned, makeSf)) %>% 
  # Calculate the area of the convex hull
  mutate(convex_hull = purrr::map(cleaned, ~st_convex_hull(st_union(.))), 
         area = purrr::map_dbl(convex_hull, st_area)) %>% 
  # Calculate the distance traveled 
  mutate(polyline = purrr::map(cleaned, ~st_linestring(st_coordinates(.))), 
         length = purrr::map_dbl(polyline, st_length))

plot(processed_days_unnest$convex_hull[[2]])
plot(processed_days_unnest$cleaned[[2]], add = TRUE, color = "red")


# Check correlation between numTrips, area, and length
cor_prep <- final_table %>% 
  filter(!is.na(numTrips))

correlation <- cor(cor_prep[, c("numTrips", "area", "length")])

# Print correlation matrix
print(correlation)



