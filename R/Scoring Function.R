library(ggplot2)
library(dplyr)
library(ggridges)
library(tidyverse)
library(targets)
library(readr)



tar_target(cleaned_ids, clean_userids(read_in))

tar_target(scored_days, scoring(cleaned_ids))

tar_target(cleaned_data, gps_points_process(scored_days))

#' Clean GPS points data by removing specified userIds.
#'
#' This function takes a tibble of GPS points and removes rows corresponding to
#' specified userIds. The userIds to be removed are hardcoded in the function.
#'
#' @param gps_points A tibble containing GPS points data.
#' 
#' @return A tibble with rows corresponding to specified userIds removed.
#'
clean_userids <- function(gps_points) {
  relevant_ids <- gps_points %>% 
    filter(!userId %in% c("5ce457e340c7ec3c62f0bf0b", 
                          "5cf80a3e39b0af75d30f8a9a", 
                          "5d012e9194fc444819b759df", 
                          "5d0be0795c9e01405be7a410", 
                          "5dd74879c275fa51872433c4", 
                          "5f4eb5c0df4cfc08a627d827", 
                          "5f600ae96ac58a28e1862544", 
                          "60be7eba0cfa734406650c33", 
                          "5ce58b8fb806a3095b02825d",
                          "5ce81b11df98d115d6a6d533",
                          "5d01492cac3695481f754b11",
                          "5d01495eac3695481f754b14",
                          "5d56c21c03448c58bbe4b6c8",
                          "5f29a58184b80f5e2521f4ee"))
  
  return(relevant_ids)
}


#' Calculate scores for relevant GPS points data.
#'
#' This function takes a tibble of relevant GPS points and calculates scores
#' based on the number and spread of GPS points during a day for each user and day.
#'
#' @param relevant_ids A tibble containing relevant GPS points data.
#'
#' @return A tibble with calculated scores for each user and day.

scoring <- function(relevant_ids) {
  scored <- relevant_ids %>% 
    mutate(
      timestamp = as_datetime(time),
      activityDay = yesterday(timestamp),
      hour = hour(timestamp),
      minute = minute(timestamp)) %>%
    # Keep these columns for analysis
    select(
      userId,
      activityDay,
      timestamp,
      hour,
      minute,
      lat,
      lon) %>% 
    # Calculate the score for the day based on number and spread of gps points
    arrange(userId, activityDay, hour) %>% 
    group_by(userId, activityDay, hour) %>%
    nest() %>% 
    ungroup() %>% 
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    mutate(
      hour_multiplier = ifelse(hour %in% 8:23, 3, 1),
      points_multiplier = case_when(
        num_points <= 500 ~ 0,
        num_points <= 1500 ~ 1,
        num_points <= 2500 ~ 2,
        TRUE ~ 3
      ),
      daily_score = hour_multiplier * points_multiplier
    ) %>% 
    group_by(userId, activityDay) %>%
    mutate(total_daily_score = sum(daily_score)) %>% 
  # Keep rows with total_daily_score >= 160
    ungroup() %>% 
    filter(total_daily_score >= 160) %>% 
    select(-c(total_daily_score, hour_multiplier, points_multiplier, daily_score, num_points)) %>% 
    unnest(cols = c(cleaned))
  
  return(scored)
}


#' Process scored GPS points data.
#'
#' This function takes a tibble of scored GPS points and performs various
#' processing steps including sampling, grouping, and creating Simple Features.
#'
#' @param scored A tibble containing scored GPS points data.
#'
#' @return A processed tibble with sampled, grouped, and transformed data.

gps_points_process <- function(scored) {
  processed <- scored %>%
    arrange(userId, activityDay, hour, minute) %>% 
    group_by(userId, activityDay, hour, minute) %>%
    slice_sample(n = 10, replace = FALSE) %>%
    
    ungroup() %>% 
    group_by(userId, activityDay) %>%
    nest() %>% 
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>%
    filter(num_points > 1000)  %>% 
    mutate(cleaned = purrr::map(cleaned, makeSf))
  
  return(processed)
}








### SCORING FUNCTION ###

mean(presliced_subset$num_points)
median(presliced_subset$num_points)
hist(presliced_subset$num_points)

# userIds that are not actual participants
user_ids_to_remove <- c("5ce457e340c7ec3c62f0bf0b", 
                        "5cf80a3e39b0af75d30f8a9a", 
                        "5d012e9194fc444819b759df", 
                        "5d0be0795c9e01405be7a410", 
                        "5dd74879c275fa51872433c4", 
                        "5f4eb5c0df4cfc08a627d827", 
                        "5f600ae96ac58a28e1862544", 
                        "60be7eba0cfa734406650c33", 
                        "5ce58b8fb806a3095b02825d",
                        "5ce81b11df98d115d6a6d533",
                        "5d01492cac3695481f754b11",
                        "5d01495eac3695481f754b14",
                        "5d56c21c03448c58bbe4b6c8",
                        "5f29a58184b80f5e2521f4ee")

presliced_data_hour <- presliced_data %>% 
  unnest(cols = c(cleaned)) %>% 
  select(-num_points) %>% 
  filter(!userId %in% user_ids_to_remove) %>%
  group_by(userId, activityDay, hour) %>%
  nest() %>% 
  ungroup() %>%
  rename(cleaned = data) %>%
  mutate(num_points = purrr::map_int(cleaned, nrow))

mean(presliced_data_hour$num_points)
median(presliced_data_hour$num_points)
hist(presliced_data_hour$num_points)


total_points_by_hour <- presliced_data_hour %>%
  group_by(hour) %>%
  summarise(total_points = sum(num_points))

# Create a histogram showing the total number of GPS 
ggplot(total_points_by_hour, aes(x = hour, y = total_points)) +
  geom_col() +
  labs(title = "Total Number of Points by Hour",
       x = "Hour",
       y = "Total Number of Points") +
  theme_minimal()


# Function to calculate a daily score
calculate_daily_score <- function(data) {
  result <- data %>%
    # sample_n(10, replace = FALSE) %>% 
    unnest(cols = c(cleaned)) %>% 
    select(-timestamp_new, num_points) %>% 
    group_by(userId, activityDay, hour) %>%
    nest() %>% 
    ungroup() %>%
    rename(cleaned = data) %>%
    mutate(num_points = purrr::map_int(cleaned, nrow)) %>% 
    mutate(
      hour_multiplier = ifelse(hour %in% 8:23, 3, 1),
      points_multiplier = case_when(
        num_points <= 500 ~ 0,
        num_points <= 1500 ~ 1,
        num_points <= 2500 ~ 2,
        TRUE ~ 3
      ),
      daily_score = hour_multiplier * points_multiplier
    ) %>% 
    group_by(userId, activityDay) %>%
    summarize(total_daily_score = sum(daily_score))
  
  return(result)
}

# Apply the function to your data
presliced_data_hour_score <- calculate_daily_score(presliced_data_hour)

# Assuming your data frame is called 'presliced_data_hour_score'
ggplot(presliced_data_hour_score, aes(x = total_daily_score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 350, by = 25)) +
  scale_y_continuous(breaks = seq(0, 4500, by = 100)) +
  labs(title = "Distribution of Total Daily Scores",
       x = "Total Daily Score",
       y = "Frequency") +
  theme_minimal()


ggplot(presliced_data_hour_score, aes(x = total_daily_score)) +
  stat_ecdf()


# summary stats for all userIds
summary_stats_all <- presliced_data_hour_score %>%
  group_by(userId) %>%
  summarize(
    num_activityDays = n_distinct(activityDay),
    avg_total_daily_score = mean(total_daily_score)
  )

ggplot(summary_stats_all, aes(x = avg_total_daily_score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Average Total Daily Scores",
       x = "Average Total Daily Score",
       y = "Frequency")


# determine total_daily_scores were above a particular threshold and the 
# corresponding unique userIds
high_scores <- presliced_data_hour_score %>% 
  filter(total_daily_score > 50)

unique_userIds_high <- distinct(high_scores, userId)

# summary stats for the high scores
summary_stats_high <- high_scores %>%
  group_by(userId) %>%
  summarize(
    num_activityDays = n_distinct(activityDay),
    avg_total_daily_score = mean(total_daily_score)
  )

ggplot(summary_stats_high, aes(x = avg_total_daily_score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(200, 350, by = 25)) +
  labs(title = "Histogram of Average Total Daily Scores",
       x = "Average Total Daily Score",
       y = "Frequency")

# create a new dataset before adding the survey data
high_scores_demo_survey_before <- high_scores %>%
  rename(date = activityDay) %>% 
  left_join(demographics, by = "userId")

# create a new dataset with the demographic data and survey data appended
high_scores_demo_survey <- high_scores %>%
  rename(date = activityDay) %>% 
  left_join(demographics, by = "userId") %>% 
  addMentalHealthResponses()


# Assuming your data frame is called high_scores_demo_survey
rows_with_blank_survey_morn <- high_scores_demo_survey %>%
  filter(is.na(Survey.Name.x))

rows_with_blank_survey_even <- high_scores_demo_survey %>%
  filter(is.na(Survey.Name.y))


# Assuming your data frame is called high_scores_demo_survey
rows_with_blank_surveys <- high_scores_demo_survey %>%
  filter((is.na(Survey.Name.x) | Survey.Name.x == "") & 
         (is.na(Survey.Name.y) | Survey.Name.y == ""))

# Calculate the percentage
percentage_blank_surveys <- nrow(rows_with_blank_surveys) / nrow(high_scores_demo_survey) * 100

# Display the result
percentage_blank_surveys


# number of rows
all <- nrow(high_scores_demo_survey)

# no morning survey
survey_morn <- nrow(high_scores_demo_survey) - nrow(rows_with_blank_survey_morn)
survey_morn/all

# no evening survey
survey_even <- nrow(high_scores_demo_survey) - nrow(rows_with_blank_survey_even)
survey_even/all

# no survey
survey_both <- nrow(high_scores_demo_survey) - nrow(rows_with_blank_surveys)
survey_both/all

table(is.na(high_scores_demo_survey$Survey.Name.x), is.na(high_scores_demo_survey$Survey.Name.y))/nrow(high_scores_demo_survey)

result <- high_scores_demo_survey %>%
  group_by(userId) %>%
  summarise(
    num_surveys = n(),
    min_date = min(date),
    max_date = max(date),
    elapsed_days = as.numeric(difftime(max_date, min_date, units = "days") +1),
    percent_comp_surveys = num_surveys / elapsed_days
  )



set.seed(10)

# Filter rows with total_daily_score >= 160
filtered_data <- presliced_data_hour_score %>%
  ungroup() %>% 
  filter(total_daily_score >= 160) %>% 
  sample_n(50, replace = FALSE) %>% 
  select( -total_daily_score)

# View the randomly selected sample
print(filtered_data)

# Assuming your tibble is called my_tibble
write_csv(filtered_data, "data/high_score_sample.csv")
