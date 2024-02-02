#' Read and Process Demographic Data
#'
#'
#' @param file_path The path to the Excel file containing demographic data.
#' @return demographics A tibble with cleaned demographic data.
#'
#' @details Reads an Excel file, renames columns, removes unnecessary columns, 
#' and relevels factor variables.

readDemographicData <- function(file_path) {
  demo_data <- readxl::read_excel(file_path) %>% 
    rename(userId = Metricwire_ID) %>% 
    select( -c(Recipient_First_Name)) %>% 
    mutate(
      initial_group = if(is.ordered(initial_group)) initial_group else relevel(as.factor(initial_group), ref = "Control"),
      prescribed_group = if(is.ordered(prescribed_group)) prescribed_group else relevel(as.factor(prescribed_group), ref = "Control"),
      gender = if(is.ordered(gender)) gender else relevel(as.factor(gender), ref = "Male"),
      race = if(is.ordered(race)) race else relevel(as.factor(race), ref = "White")
    )
}


#' Find unique values in a specified column.
#'
#' This function takes a data frame and a column name as input,
#' and returns the unique userIds that scored high.
#'
#' @param data A data frame containing the data.
#' @param column_name A character string specifying the column name.
#'
#' @return A vector of unique values in the specified column.

find_high_scorers <- function(data, column_name) {
  column_name <- "userId"
  unique_userIds <- unique(data[[column_name]])
  
  return(unique_userIds)
}


#' Process demo data by filtering based on high scorers.
#'
#' This function takes a demo data frame and a vector of high scorers' userIds,
#' filters the data to include only rows with userIds present in the high scorers vector,
#' and returns the processed data frame.
#'
#' @param demo_data A data frame containing demo data.
#' @param high_scorers A vector of userIds corresponding to high scorers.
#'
#' @return A data frame with rows containing userIds present in the high scorers vector.

demo_data_process <- function(demo_data, high_scorers) {
  processed_data <- demo_data %>%
    filter(userId %in% high_scorers)
  
  return(processed_data)
}


#' Create a Data Frame with User IDs and Dates
#'
#'
#' @param demographics A data frame containing user demographics with a 
#' 'userId' column.
#' @param start_date The start date of the date range, in "YYYY-MM-DD" format.
#' @param end_date The end date of the date range, in "YYYY-MM-DD" format.
#' @return frame A data frame with userIds and dates.
#'
#' @details The function takes a data frame of user demographics and generates 
#' a data frame with all possible combinations of user IDs and dates within the 
#' specified date range.

make_frame <- function(demographics, start_date, end_date){
  userIds <- demographics$userId
  dates <- seq(as.Date(end_date), as.Date(start_date), by = "-1 day")
  data_frame <- expand.grid(userId = userIds, date = dates)
  data_frame <- arrange(data_frame, userId)
  data_frame <- data_frame[!is.na(data_frame$userId) & !is.na(data_frame$date), ]
  return(data_frame)
}


#' Add Demographic Data
#'
#'
#' @param frame The tibble to which demographic data will be added.
#' @param demographics The tibble containing demographic data.
#' @return demo_table A tibble with added demographic data.
#'
#' @details Performs a left join on the 'userId' column, adding demographic 
#' data to the data frame.

addDemographicData <- function(tibble, demographics) {
  left_join(tibble, demographics, by = 'userId', relationship ="many-to-many")
}


#' Add Mental Health Responses
#'
#'
#' @param demo_table To which mental health survey responses will be added.
#' @return survey_table With added mental health survey responses.
#'
#' @details Reads CSV files containing morning and evening survey responses, 
#' renames columns, converts date formats, and joins the data to demo_table.

addMentalHealthResponses <- function(tibble){
  morningResponses <- read.csv("data/mental_surveys/Morning_Survey_220223.csv") %>%
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
    mutate(date = yesterday(date)) %>% 
    select(
      userId,
      Survey.Name,
      date,
      `bed_time_q1`:`safety_request_q17_morn` 
    ) %>% 
  group_by(userId, date) %>%
    slice(1)
  
  eveningResponses <- read.csv("data/mental_surveys/Evening_Survey_220223.csv") %>% 
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
    mutate(date = yesterday(date)) %>% 
    select(
      userId,
      Survey.Name,
      date,
      `energy_day_q1`:`safety_request_q36_even` 
    ) %>% 
    mutate(suicidal_ideation_q31_even = case_when(
      suicidal_ideation_q31_even == "Yes" ~ TRUE,
      suicidal_ideation_q31_even == "No" ~ FALSE,
      TRUE ~ as.logical(NA)
    )) %>% 
    group_by(userId, date) %>%
    slice(1)
  
  morning <- left_join(tibble, morningResponses, by = c('userId', 'date'), relationship = "one-to-one")
  evening <- left_join(morning, eveningResponses, by = c('userId', 'date'), relationship = "one-to-one")
  
  evening <- evening %>% 
    mutate(activityDay = date)
  
  return(evening)
}


#' Combine Survey Data with Activities Data
#'
#'
#' @param survey_table The survey data as a tibble, with demographic data.
#' @param activity_types The activities data as a tibble.
#' @return complete_table With combined demographic, survey, and activity data.
#'
#' @details Performs a left join on 'userId' and 'date' columns to combine 
#' survey and activities data.

combine_data <- function(survey, activities){
  all_data <- left_join(survey, activities, by = c('userId', 'activityDay'))
  return(all_data)
}


#' Clean a Table to Keep Relevant Rows
#'
#'
#' @param complete_table The input table as a tibble.
#' @return clean_comp_table With rows containing either morning or evening 
#' survey data.
#'
#' @details Filters the table to retain rows with either where either the
#' evening survey or the morning survey was taken on a given day.

clean_table <- function(table){
  clean <- table %>%
    filter(status != "Withdrawn") %>% 
    filter(!is.na(Survey.Name.x) | !is.na(Survey.Name.y) | !is.na(numTrips))
    
  return(clean)
}
