# Figure out what is causing the missingness ...

library(dplyr)
library(tibble)
library(lubridate)
library(tidyverse)
library(reshape)
library(calendR)

# WHICH userIDs are there from the list of gps .csvs
folder_path <- "data/caps_data"

csv_files <- list.files(folder_path, pattern = ".csv")

csv_file_names <- sub("\\.csv$", "", csv_files)

# WHICH userIds from the demographics
demo_tb <- demographics

# Extract a single column into a list
demo_column_name <- "userId"
demo_column_values <- demo_tb %>%
  pull(!!demo_column_name)
unique_demo_userIds <- unique(demo_column_values)
unique_demo_userIds <- na.omit(demo_column_values)
# unique_demo_userIds <- data.frame(Source = "demo_tb", Unique_Ids = unique_demo_userIds)

# WHICH unique userIds that made trips
gps_tb <- demographics_table

# Extract a single column into a list
gps_column_name <- "userId"
gps_column_values <- gps_tb %>%
  pull(!!gps_column_name)
unique_gps_userIds <- unique(gps_column_values)
unique_gps_userIds <- na.omit(unique_gps_userIds)
# unique_gps_userIds <- data.frame(Source = "gps_tb", Unique_Ids = unique_gps_userIds)

# WHICH unique userIds that filled out surveys
survey_tb <- final_table

# Extract a single column into a list
survey_column_name <- "userId"
survey_column_values <- survey_tb %>%
  pull(!!survey_column_name)
unique_survey_userIds <- unique(survey_column_values)
unique_survey_userIds <- na.omit(unique_survey_userIds)
# unique_survey_userIds <- data.frame(Source = "survey_tb", Unique_Ids = unique_survey_userIds)

# Combine userId values from different tables into a single data frame
combined_data <- data.frame(
  userId = c(unique_demo_userIds, csv_file_names, unique_gps_userIds, unique_survey_userIds),
  source = c(rep("demo_tb", length(unique_demo_userIds)),
             rep("csvs", length(csv_file_names)),
             rep("gps_tb", length(unique_gps_userIds)),
             rep("survey_tb", length(unique_survey_userIds)))
)

# Find missing userIds in each source
missing_userIds_demo <- setdiff(unique_demo_userIds, c(unique_gps_userIds,unique_survey_userIds))
missing_userIds_csvs1 <- setdiff(csv_file_names, unique_demo_userIds)
missing_userIds_csvs2 <- setdiff(unique_demo_userIds, csv_file_names)
missing_userIds_gps <- setdiff(unique_gps_userIds, unique_demo_userIds)
missing_userIds_survey <- setdiff(unique_survey_userIds, unique_demo_userIds)

# Create data frames to display missing userIds for each source
missing_users_demo_df <- data.frame(Source = "demo_tb", Missing_Ids = 0)
missing_userIds_csvs1_df <- data.frame(Source = "csvs", Missing_Ids = missing_userIds_csvs1)
missing_userIds_csvs2_df <- data.frame(Source = "csvs_demo", Missing_Ids = missing_userIds_csvs2)
missing_users_gps_df <- data.frame(Source = "gps_tb", Missing_Ids = missing_userIds_gps)
missing_users_survey_df <- data.frame(Source = "survey_tb", Missing_Ids = missing_userIds_survey)

# Combine the data frames into a single table
missing_userIds_table <- rbind(missing_users_demo_df, missing_users_gps_df, 
                               missing_users_survey_df, missing_userIds_csvs1_df, 
                               missing_userIds_csvs2_df)

# View the table
print(missing_userIds_table)


# MORE ANALYSIS ON THE DATA

# 34,310 rows from 94 users * 365 days
users_demo <- demo_data %>% group_by(userId) %>% summarise(n = n())
# 34, 906 rows (there are 596 duplicates or repeats)
users_survey <- survey_data %>% group_by(userId) %>% summarise(n = n())

# check for duplicates based on userId and date in the survey data
duplicates <- survey_data[duplicated(survey_data[c("userId", "date")]), ]
unique_duplicates <- unique(duplicates[c("userId", "date")])
print(unique_duplicates)

# Determine with userIds have GPS data and survey responses based on the complete_table
comp_data_1 <- complete_table %>% 
  group_by(userId) %>% 
  summarise(n = n(), p_morn = sum(!is.na(Survey.Name.x))/n, p_even = sum(!is.na(Survey.Name.y))/n, p_gps = sum(!is.na(numTrips))/n) 
 
# Filter for rows where GPS data is 0 but either p_morn or p_even is not 0
table1 <- comp_data_1 %>%
  filter(p_gps == 0 & (p_morn != 0 | p_even != 0))

# Filter for rows where GPS data is not 0 but either p_morn or p_even is 0
table2 <- comp_data_1 %>%
  filter(p_gps != 0 & (p_morn == 0 | p_even == 0))

# Filter for rows where GPS data is not 0 and either p_morn or p_even is not 0
table3 <- comp_data_1 %>%
  filter(p_gps != 0 & (p_morn != 0 | p_even != 0))

# Filter for rows where all three columns are equal to 0
table4 <- comp_data_1 %>%
  filter(p_gps == 0 & p_morn == 0 & p_even == 0)


# Determining compliance with surveys

# list of userIds that we not included in the study
user_ids_to_remove <- c("5ce457e340c7ec3c62f0bf0b",
                        "5cf80a3e39b0af75d30f8a9a",
                        "5d012e9194fc444819b759df",
                        "5d0be0795c9e01405be7a410",
                        "5dd74879c275fa51872433c4",
                        "5f4eb5c0df4cfc08a627d827",
                        "5f600ae96ac58a28e1862544",
                        "60be7eba0cfa734406650c33")

# Group the data by 'User.Id' and find the first and last survey dates
even_survey_data <- read.csv("data/mental_surveys/Evening_Survey_220223.csv") %>% 
  rename(userId = User.Id, date = Survey.Started.Date) %>% 
  mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
  group_by(userId) %>% 
  summarise(FirstSurveyDate = min(date),
            LastSurveyDate = max(date),
            total_responses = n()) %>%
  arrange(userId) %>%
  filter(!userId %in% user_ids_to_remove) %>% 
  mutate(DaysBetween = as.numeric(difftime(LastSurveyDate, FirstSurveyDate, units = "days")))
 

as.data.frame(even_survey_data)

