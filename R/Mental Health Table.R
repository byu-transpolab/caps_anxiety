#' @param accurate_tibble target
#' @return a table including ID, date, sf, algorithm, and the 
#' number of clusters as its own column

addNumTrips <- function(df){
  tibble <- as_tibble(df) %>%
    select(userId, date, cleaned, algorithm) %>%
    group_by(userId, date) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
}


#' @param output from addNumTrips function
#' @return a table including ID, date, sf, algorithm, the
#' number of total clusters as its own column, and how many of those
#'  clusters were at a park, library, or grocery store

addTripType <- function(tibble, parksSf, grocerySf, librarySf){
  
  t2 <- tibble %>%
    filter(!is.character(algorithm)) %>%
    select(-c(cleaned))
  
  # add more land use location variables
  t2$park = purrr::map_int(t2$algorithm, ~add_location(.x, variable = id, sf = parksSf))
  t2$grocery = purrr::map_int(t2$algorithm, ~add_location(.x, variable = SITE_NAME, sf = grocerySf))
  t2$library = purrr::map_int(t2$algorithm, ~add_location(.x, variable = fid, sf = librarySf))

  t2
}

# Function to add a location
add_location <- function(x, variable, sf){
  if(is.null(nrow(x))) {
    a <- NA
  } else {
    a <- st_join(x, sf) %>% 
      st_set_geometry(NULL) %>% 
      ungroup() %>% 
      summarise(s = sum(!is.na({{variable}})) ) %>% 
      pull(s)
  }
   
  return(a)
}

# Add Mental Health Responses and Demographic Data
addMentalHealthResponses <- function(tibble){
  morningResponses <- read.csv("data/mental_surveys/Morning_Survey_220223.csv") %>%
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
    select(
      userId,
      Survey.Name,
      date,
      `bed_time_q1`:`safety_request_q17_morn` 
    ) 
  
  eveningResponses <- read.csv("data/mental_surveys/Evening_Survey_220223.csv") %>% 
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
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
    ))
  
  morning <- full_join(tibble, morningResponses, by = c('userId', 'date'), relationship ="many-to-many")
  evening <- full_join(morning, eveningResponses, by = c('userId', 'date'), relationship ="many-to-many")
  
  return(evening)
}

addDemographicData <- function(tibble) {
  demographic <- readxl::read_excel("data/mental_surveys/Demographic_Breakdown.xlsx") %>%
    rename(userId = Metricwire_ID) %>% 
    select( -c(Recipient_First_Name))
    
  full_join(tibble, demographic, by = 'userId')
}