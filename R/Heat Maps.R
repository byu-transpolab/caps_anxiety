library(dplyr)
library(tibble)
library(lubridate)
library(tidyverse)
library(reshape)


# SURVEY HEAT MAP

# Read the CSV file
even_survey_map <- read_csv("data/mental_surveys/Evening_Survey_220223.csv", col_select = c("User Id", "Survey Started Date"))

# Rename the columns using colnames
colnames(even_survey_map) <- c("userId", "date")

# Continue with the data processing
even_survey_map <- even_survey_map %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarise(Total_Responses = n())

# Read the CSV file
morn_survey_map <- read_csv("data/mental_surveys/Morning_Survey_220223.csv", col_select = c("User Id", "Survey Started Date"))

# Rename the columns using colnames
colnames(morn_survey_map) <- c("userId", "date")

# Continue with the data processing
morn_survey_map <- morn_survey_map %>%
  mutate(date = dmy(date)) %>%
  group_by(date) %>%
  summarise(Total_Responses = n())

survey_map <- left_join(morn_survey_map, even_survey_map, by = join_by(date)) %>% 
  mutate(Surveys = coalesce(Total_Responses.x, 0) + coalesce(Total_Responses.y, 0)) %>% 
  select(date, Surveys)

print(survey_map)

# Create a heat map calendar
survey_map$MonthYear <- format(survey_map$date, "%Y-%m")
ggplot(survey_map, aes(x = MonthYear, y = format(date, "%d"))) +
  geom_tile(aes(fill = Surveys)) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(
    title = "Heat Map Calendar for Surveys",
    x = "Month",
    y = "Day",
    fill = "Surveys"
  ) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Create a histogram
ggplot(survey_map, aes(x = date, y = Surveys)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(
    x = "Date",
    y = "Number of Surveys",
    title = "Surveys Over Time"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# GPS HEAT MAP

gps_map <- activity_types %>%
  group_by(date) %>%
  summarise(Activities = sum(numTrips))

# Print the resulting tibble
print(gps_map)

# Create a heat map calendar
gps_map$MonthYear <- format(gps_map$date, "%Y-%m")
ggplot(gps_map, aes(x = MonthYear, y = format(date, "%d"))) +
  geom_tile(aes(fill = Activities)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    title = "Heat Map Calendar for Activities",
    x = "Month",
    y = "Day",
    fill = "Activities"
  ) +
  coord_fixed(ratio = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Create a histogram
ggplot(gps_map, aes(x = date, y = Activities)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Activities Over Time",
    x = "Date",
    y = "Total Number of Trips"
  )

# Combine and melt survey and GPS data
together <- as.data.frame(full_join(gps_map %>% select(-MonthYear), survey_map %>% select(-MonthYear), by = join_by(date)))
together <- melt(together, id = "date")

# Create a histogram with different colors
ggplot(data = together, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_fill_manual(values = c("blue", "red")) +  
  labs(
    title = "Combined Plot of Activities and Survey Responses Over Time",
    x = "Date",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## GPS POINTS ANALYSIS

# Number of GPS Points recorded on the raw csv files over time
ggplot(data = gps_points, aes(x = date, y = GPS_Points)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(
    title = "GPS Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of GPS Points after the data was cleaned
filtered_data <- cleaned_data %>% 
  select(date, num_points) %>% 
  group_by(date) %>% 
  summarise(Filtered_Points = sum(num_points))

ggplot(data = filtered_data, aes(x = date, y = Filtered_Points)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(
    title = "All Points Over Time",
    x = "Date",
    y = "# of GPS Points"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Compare the number of points from the raw data and from the cleaned data
# Combine and melt survey and GPS data
combine_gps <- as.data.frame(full_join(gps_points, filtered_data, by = join_by(date)))
combine_gps <- melt(combine_gps, id = "date")

# Create a histogram with different colors
ggplot(data = combine_gps, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_fill_manual(values = c("forestgreen", "purple")) +
  labs(
    title = "Combined Plot of GPS Points Over Time",
    x = "Date",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

