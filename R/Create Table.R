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


#' Count the number of morning and evening survey days.
#'
#' This function takes a tibble with survey information and calculates the counts
#' of morning survey days, evening survey days, and days with both surveys filled out.
#'
#' @param survey A tibble containing survey information with columns Survey.Name.x and Survey.Name.y.
#'
#' @return A tibble with counts of morning survey days, evening survey days, and days
#' with both morning and evening surveys filled out.

summarize_survey_data <- function(survey) {
  summary <- survey %>%
    mutate(
      morning_survey = !is.na(Survey.Name.x),
      evening_survey = !is.na(Survey.Name.y),
      both_surveys = !is.na(Survey.Name.x) & !is.na(Survey.Name.y)
    ) %>%
    summarize(
      morning_survey_days = sum(morning_survey),
      evening_survey_days = sum(evening_survey),
      both_surveys_days = sum(both_surveys)
    )
  
  return(summary)
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

clean_final_table <- function(table){
  clean <- table %>%
    filter(status != "Withdrawn") %>% 
    filter(!(is.na(Survey.Name.x) & is.na(Survey.Name.y) & is.na(numTrips)))
    
  return(clean)
}


#' Count the number of days based on survey and trips conditions.
#'
#' This function takes a tibble with survey information and trips data and calculates
#' the counts of days based on specified conditions:
#' - Days with both morning survey (Survey.Name.x) and evening survey (Survey.Name.y) not NA, and numTrips not NA.
#' - Days where either morning survey (Survey.Name.x) or evening survey (Survey.Name.y) is not NA (but not both),
#'   and numTrips is not NA.
#' - Days where both morning survey (Survey.Name.x) and evening survey (Survey.Name.y) are NA, but numTrips is not NA.
#'
#' @param final A tibble containing survey information with columns Survey.Name.x,
#' Survey.Name.y, and numTrips.
#'
#' @return A tibble with counts of days based on the specified conditions.

summarize_final_data <- function(final) {
  summary <- final %>%
    mutate(
      condition1 = !is.na(Survey.Name.x) & !is.na(Survey.Name.y) & !is.na(numTrips),
      condition2 = (!is.na(Survey.Name.x) | !is.na(Survey.Name.y)) & !(!is.na(Survey.Name.x) & !is.na(Survey.Name.y)) & !is.na(numTrips),
      condition3 = is.na(Survey.Name.x) & is.na(Survey.Name.y) & !is.na(numTrips)
    ) %>%
    summarize(
      condition1_days = sum(condition1),
      condition2_days = sum(condition2),
      condition3_days = sum(condition3)
    )
  
  return(summary)
}

