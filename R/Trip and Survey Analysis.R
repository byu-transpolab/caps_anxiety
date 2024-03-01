library(targets)
library(tidyverse)

# Load summary tables
tar_load(raw_data_summary)
tar_load(filtered_data_summary)
tar_load(scored_data_summary)
tar_load(prepared_data_summary)
tar_load(trip_data_summary)
tar_load(survey_data_summary)
tar_load(final_data_summary)

# Load targets related to processing the gps points
tar_load(preprocessed)
tar_load(scored_days)
tar_load(prepared_data)
tar_load(num_trips)
tar_load(activity_types)

# Load targets related to the demographics and surveys
tar_load(demographics)
tar_load(survey_data)

# Load combined tables
tar_load(complete_table)
tar_load(final_table)


get_unique_userIds <- function(data) {
  unique_userIds <- unique(data$userId)
  return(unique_userIds)
}

final <- get_unique_userIds(final_table)
complete <- get_unique_userIds(complete_table)
survey <- get_unique_userIds(survey_data)
demographs <- get_unique_userIds(demographics)
activity <- get_unique_userIds(activity_types)
prepared <- get_unique_userIds(prepared_data)
scored <- get_unique_userIds(scored_days)
preprocess <- get_unique_userIds(preprocessed)
raw <- get_unique_userIds(read_in)



dim(preprocessed) #392731776 rows with 88 user
dim(scored_days) #85274679 with 42 users, reduces to 21.7% of the original GPS data and 48% of the original number of users
dim(prepared_data) #1012 userId-activityDay with 42 users
dim(num_trips) #1012
dim(activity_types) #1012 userId-activityDay

dim(demographics) #94 users
dim(survey_data) #97384 

dim(complete_table) #1012 userId-activityDay with 42 users
dim(final_table) #882 userId-activityDay with 33 users


# 18 of the 94 users withdrew from the study
withdrawn_demo <- sum(demographics$status == "Withdrawn")
print(withdrawn_demo)

# 130 of the high scored days are users who withdrew
withdrawn_count <- sum(complete_table$status == "Withdrawn")
print(withdrawn_count)


unique_scored <- length(unique(scored_days$userId))
print(unique_scored)

unique_preprocessed <- length(unique(preprocessed$userId))
print(unique_preprocessed)

unique_prepared <- length(unique(prepared_data$userId))
print(unique_prepared)

unique_complete <- length(unique(complete_table$userId))
print(unique_complete)

unique_final <- length(unique(final_table$userId))
print(unique_final)



prepared_data %>%
  ggplot(aes(x = factor(userId))) +
  geom_bar(stat = "count", fill = "red", color = "black") +
  labs(x = "userId", y = "Number of Activity-Days", title = "Histogram of Number of Activity Days per UserID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(table(prepared_data$userId)), 10))

final_table %>%
  ggplot(aes(x = factor(userId))) +
  geom_bar(stat = "count", fill = "blue", color = "black") +
  labs(x = "userId", y = "Number of Activity-Days", title = "Histogram of Number of Activity Days per UserID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(table(final_table$userId)), 10))

final_table %>%
  ggplot(aes(x = numTrips)) +
  geom_bar(stat = "count", fill = "blue", color = "black") +
  labs(x = "Number of Trips", y = "Frequency", title = "Histogram of Number of Trips per Activity Day") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, max(table(final_table$numTrips)), 10))

final_table %>% 
ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
  facet_wrap(final_table$park, final_table$library, final_table$grocery, scales = "free") +
  labs(title = "Distribution of Locations", x = "Value", y = "Frequency") +
  theme_bw()





final_table_long <- tidyr::gather(final_table, key = "location_type", value = "value", park, library, grocery)

# Filter out 0 values
final_table_filtered <- final_table_long %>%
  filter(value != 0)

# Histograms with facets for non-zero values
ggplot(final_table_filtered, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "darkblue", color = "black") +
  facet_wrap(~ location_type, scales = "free") +
  labs(title = "Distribution of Locations (Excluding 0 Values)", x = "Number of Visits to a Certain Location", y = "Frequency") +
  scale_x_continuous(breaks = seq(1, max(final_table_filtered$value), by = 1)) +
  theme_bw()






