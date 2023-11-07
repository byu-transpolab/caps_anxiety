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


### 
###


# Determining compliance with surveys

# EVENING ~ list of userIds that we not included in the study
user_ids_to_remove_even <- c("5ce457e340c7ec3c62f0bf0b", 
                             "5cf80a3e39b0af75d30f8a9a", 
                             "5d012e9194fc444819b759df", 
                             "5d0be0795c9e01405be7a410", 
                             "5dd74879c275fa51872433c4", 
                             "5f4eb5c0df4cfc08a627d827", 
                             "5f600ae96ac58a28e1862544", 
                             "60be7eba0cfa734406650c33")

# Group the data by 'User.Id' and find the first and last survey dates
# Read the CSV file and rename columns
even_survey_data <- read_csv("data/mental_surveys/Evening_Survey_220223.csv", col_select = c("User Id", "Survey Started Date"))

# Rename the columns
colnames(even_survey_data) <- c("userId", "date")

# Continue with the data processing
even_survey_data <- even_survey_data %>%
  mutate(date = dmy(date)) %>%
  group_by(userId) %>%
  summarise(
    First_Survey_Date = min(date),
    Last_Survey_Date = max(date),
    Total_Responses = n()
  ) %>%
  arrange(userId) %>%
  filter(!userId %in% user_ids_to_remove_even) %>%
  mutate(Days_Between = as.numeric(difftime(Last_Survey_Date, First_Survey_Date, units = "days")) + 1) %>%
  mutate(Per_Resp_Even = Total_Responses / Days_Between)

# MORNING ~ list of userIds that we not included in the study
user_ids_to_remove_morn <- c("5ce457e340c7ec3c62f0bf0b",
                             "5cf80a3e39b0af75d30f8a9a",
                             "5d012e9194fc444819b759df",
                             "5d0be0795c9e01405be7a410",
                             "5f4eb5c0df4cfc08a627d827",
                             "5f600ae96ac58a28e1862544",
                             "60be7eba0cfa734406650c33",
                             "5ce58b8fb806a3095b02825d",
                             "5ce81b11df98d115d6a6d533",
                             "5d01492cac3695481f754b11",
                             "5d01495eac3695481f754b14",
                             "5d56c21c03448c58bbe4b6c8")

# Group the data by 'User.Id' and find the first and last survey dates

# Read the CSV file and rename columns
morn_survey_data <- read_csv("data/mental_surveys/Morning_Survey_220223.csv", col_select = c("User Id", "Survey Started Date"))

# Rename the columns
colnames(morn_survey_data) <- c("userId", "date")

morn_survey_data <- morn_survey_data %>%
  mutate(date = dmy(date)) %>%
  group_by(userId) %>%
  summarise(
    First_Survey_Date = min(date),
    Last_Survey_Date = max(date),
    Total_Responses = n()
  ) %>%
  arrange(userId) %>%
  filter(!userId %in% user_ids_to_remove_morn) %>%
  mutate(Days_Between = as.numeric(difftime(Last_Survey_Date, First_Survey_Date, units = "days")) + 1) %>%
  mutate(Per_Resp_Morn = Total_Responses / Days_Between)

# Combined table with userId and the percent of responses from each userId
survey_days <- inner_join(even_survey_data, morn_survey_data, by = "userId") 
  
survey_percentages <- survey_days %>% 
  select(userId, Per_Resp_Even, Per_Resp_Morn) 

# looking at the GPS data
gps_days <- activity_types %>%
  group_by(userId) %>%
  summarize(Minimum_Date = min(date), 
            Maximum_Date = max(date),
            Total_Activity_Days = n()) %>%
  mutate(Days_Between = as.numeric(difftime(Maximum_Date, Minimum_Date, units = "days")) + 1) %>% 
  mutate(Per_GPS_Days = Total_Activity_Days/Days_Between)

gps_percentages <- gps_days %>% 
  select(userId, Per_GPS_Days) 

# combined table with the survey compliance and gps data based on the number 
# of days that there is data
all_data <- left_join(survey_percentages, gps_percentages, by = join_by(userId))


### MASTER SURVEY

# Calculate the combined number of days
ultimate_start_end_dates <- survey_days %>%
  mutate(
    Start_Date = pmin(First_Survey_Date.x, First_Survey_Date.y),
    End_Date = pmax(Last_Survey_Date.x, Last_Survey_Date.y),
    Combined_Dates = as.numeric(difftime(End_Date, Start_Date, units = "days")) + 1
  ) %>% 
  select(userId, Start_Date, End_Date, Combined_Dates, Per_Resp_Even, Per_Resp_Morn)

# Determine overlapping days
overlapping_days <- survey_days %>%
  mutate(
    Overlapping_Start_Date = pmax(First_Survey_Date.x, First_Survey_Date.y),
    Overlapping_End_Date = pmin(Last_Survey_Date.x, Last_Survey_Date.y),
    Overlapping_Days = as.numeric(difftime(Overlapping_End_Date, Overlapping_Start_Date, units = "days")) + 1
  ) %>% 
  select(userId, Overlapping_Start_Date, Overlapping_End_Date, Overlapping_Days)

survey_date_range <- left_join(ultimate_start_end_dates, overlapping_days, by = join_by(userId)) %>% 
  mutate(Date_Coverage = Overlapping_Days/Combined_Dates)
  

### MASTER SURVEY w/ GPS

# Combine the data frames by userId
combined_data <- left_join(survey_date_range, gps_days, by = "userId")

# Calculate the combined number of days
ultimate_start_end_dates1 <- combined_data %>%
  mutate(
    com_start_date = pmin(Start_Date, Minimum_Date),
    com_end_date = pmax(End_Date, Maximum_Date),
    com_days = as.numeric(difftime(com_end_date, com_start_date, units = "days")) + 1
  ) %>% 
  select(userId, Per_Resp_Even, Per_Resp_Morn, Date_Coverage, Per_GPS_Days, com_start_date, com_end_date, com_days)

# Determine overlapping days
overlapping_days1 <- combined_data %>%
  mutate(
    com_ol_start_date = pmax(Start_Date, Minimum_Date),
    com_ol_end_date = pmin(End_Date, Maximum_Date),
    com_ol_days = as.numeric(difftime(com_ol_end_date, com_ol_start_date, units = "days")) + 1
  ) %>% 
  mutate(com_ol_days = ifelse(com_ol_days < 0, 0, com_ol_days)) %>% 
  select(userId, com_ol_start_date, com_ol_end_date, com_ol_days)

gps_survey_date_range <- left_join(ultimate_start_end_dates1, overlapping_days1, by = join_by(userId)) %>% 
  mutate(Date_Coverage_All = com_ol_days/com_days) %>% 
  select(userId, Per_Resp_Morn, Per_Resp_Even, Date_Coverage, Per_GPS_Days, Date_Coverage_All)


### EXAMPLE ~ NO OVERLAP ###

# Create a table with the userIds that do not have any overlap in their GPS data and Survey data
users_with_zero_coverage <- gps_survey_date_range %>%
  filter(Date_Coverage_All == 0) %>%
  select(userId, Date_Coverage_All)

user_tibbles <- list()

for (user_id in users_with_zero_coverage$userId) {
  single_user_survey <- ultimate_start_end_dates %>%
    filter(userId == user_id)
  
  single_user_gps <- gps_days %>%
    filter(userId == user_id) %>%
    select(-Total_Activity_Days)
  
  user_tibble <- left_join(single_user_survey, single_user_gps, by = join_by(userId))
  
  user_tibbles <- c(user_tibbles, list(user_tibble))
}

# Combine the data frames and rename columns
zero_coverage_tibble <- bind_rows(user_tibbles)

# Rename the columns
colnames(zero_coverage_tibble) <- c("userId", "Survey_Start", "Survey_End", "Survey_Days", 
  "Percent_Survey_Even", "Percent_Survey_Morn", 
  "GPS_Start", "GPS_End", "GPS_Days", "Percent_GPS")

print(zero_coverage_tibble)


###
###

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


