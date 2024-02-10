library(ggplot2)
library(dplyr)
library(ggridges)
library(tidyverse)
library(targets)

# Save the new cleaned_data object for later
original_object <- cleaned_data
saveRDS(original_object, file = "cleaned_data_20_1000.rds")
new_object <- readRDS("cleaned_data_20_1000.rds")


# Fix the column names of the datasets to use for plotting
num_gps_points_plot <- num_gps_points %>% 
  rename(num_points = GPS_Points)

presliced_data_plot <- presliced_data %>% 
  mutate(date = activityDay) %>% 
  select(date, num_points) %>% 
  group_by(date) %>% 
  summarise(num_points = sum(num_points))

sliced_data_plot <- sliced_data %>% 
  mutate(date = activityDay) %>% 
  select(date, num_points) %>% 
  group_by(date) %>% 
  summarise(num_points = sum(num_points))

filtered_data_plot <- cleaned_data %>% 
  mutate(date = activityDay) %>% 
  select(date, num_points) %>% 
  group_by(date) %>% 
  summarise(num_points = sum(num_points))
  
  
### GPS POINTS ANALYSIS ###

# Number of GPS Points recorded on the raw csv files over time
ggplot(data = num_gps_points_plot, aes(x = date, y = num_points)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(
    title = "Raw GPS Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of GPS Points before slicing the data
ggplot(data = presliced_data_plot, aes(x = date, y = num_points)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    title = "Presliced GPS Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of GPS Points after slicing the data
ggplot(data = sliced_data_plot, aes(x = date, y = num_points)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Sliced GPS Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of GPS Points after filtering the data
ggplot(data = filtered_data_plot, aes(x = date, y = num_points)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(
    title = "Filtered GPS Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### CREATE COMBINED PLOTS ###

# Combine the datasets and create a new column for dataset identification
combined_raw_presliced <- rbind(transform(num_gps_points_plot, dataset = "num_gps_points_plot"),
                          transform(presliced_data_plot, dataset = "presliced_data_plot"))

# Create a ggplot bar chart with overlain data
ggplot(combined_raw_presliced, aes(x = date, y = num_points, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("num_gps_points_plot" = "forestgreen", "presliced_data_plot" = "purple")) +
  labs(title = "Comparison of num_gps_points and presliced_points",
       x = "Date",
       y = "Number of Points") +
  theme_minimal()

# Combine the datasets and create a new column for dataset identification
combined_presliced_sliced <- rbind(transform(presliced_data_plot, dataset = "presliced_data_plot"),
                              transform(sliced_data_plot, dataset = "sliced_data_plot"))

# Create a ggplot bar chart with overlain data
ggplot(combined_presliced_sliced, aes(x = date, y = num_points, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("presliced_data_plot" = "purple", "sliced_data_plot" = "blue")) +
  labs(title = "Comparison of sliced_points and presliced_points",
       x = "Date",
       y = "Number of Points") +
  theme_minimal()

# Combine the datasets and create a new column for dataset identification
combined_sliced_filtered <- rbind(transform(sliced_data_plot, dataset = "sliced_data_plot"),
                                   transform(filtered_data_plot, dataset = "filtered_data_plot"))

# Create a ggplot bar chart with overlain data
ggplot(combined_sliced_filtered, aes(x = date, y = num_points, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("sliced_data_plot" = "blue", "filtered_data_plot" = "lightgreen")) +
  labs(title = "Comparison of sliced_points and filtered_points",
       x = "Date",
       y = "Number of Points") +
  theme_minimal()

# Combine the datasets and create a new column for dataset identification
combined_raw_filtered <- rbind(transform(num_gps_points_plot, dataset = "num_gps_points_plot"),
                                  transform(filtered_data_plot, dataset = "filtered_data_plot"))

# Create a ggplot bar chart with overlain data
ggplot(combined_raw_filtered, aes(x = date, y = num_points, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("num_gps_points_plot" = "forestgreen", "filtered_data_plot" = "lightgreen")) +
  labs(title = "Comparison of num_gps_points and filtered_points",
       x = "Date",
       y = "Number of Points") +
  theme_minimal()


### GGJOY plots ###

# mutate(hour_minute = format(timestamp_new, format = "%H:%M"))

set.seed(24)

presliced_subset <- presliced_data %>%
  sample_n(30, replace = FALSE) %>% 
  unnest(cols = c(cleaned)) %>% 
  select(-num_points) %>% 
  group_by(userId, activityDay, hour) %>%
  nest() %>% 
  ungroup() %>%
  rename(cleaned = data) %>%
  mutate(num_points = purrr::map_int(cleaned, nrow))

# Find the maximum number of GPS points across all users
max_num_points <- max(presliced_subset$num_points)

# Create the line plots with three columns of facets
ggplot(presliced_subset, aes(x = hour, y = num_points, color = userId)) +
  geom_line(linewidth = 1) +
  labs(title = "Number of GPS points by hour for each userId",
       x = "Hour",
       y = "Number of GPS Points") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ userId, scales = "free_y", ncol = 3) +
  ylim(0, max_num_points)

# Create bar plots
ggplot(presliced_subset, aes(x = hour, y = num_points, fill = userId)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "Number of GPS points by hour for each userId",
       x = "Hour",
       y = "Number of GPS Points") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ userId, scales = "free_y", ncol = 3) +
  ylim(0, max_num_points)

#GGRIDGES Plot
ggplot(presliced_subset, aes(x = hour, y = userId)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

