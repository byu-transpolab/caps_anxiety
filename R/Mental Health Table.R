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

# Create a vector of the survey questions that were repeated in the morning and in the evening
morning <- read.csv("data/mental_surveys/Morning_Survey_220223.csv")
start_column <- which(names(morning) == "Have.you.thought.about.killing.yourself.in.the.past.12.hours.or.since.you.last.took.a.survey.")
end_column <- which(names(morning) == "Would.you.like.a.member.of.the.research.team.to.reach.out.to.you.because.you.are.feeling.unsafe.")
selected_columns <- names(morning)[start_column:end_column]
morning_columns <- as.vector(selected_columns)

evening <- read.csv("data/mental_surveys/Evening_Survey_220223.csv")
start_column <- which(names(evening) == "Have.you.thought.about.killing.yourself.in.the.past.12.hours.or.since.you.last.took.a.survey.")
end_column <- which(names(evening) == "Would.you.like.a.member.of.the.research.team.to.reach.out.to.you.because.you.are.feeling.unsafe.")
selected_columns <- names(evening)[start_column:end_column]
evening_columns <- as.vector(selected_columns)

# Add Mental Health Responses and Demographic Data
addMentalHealthResponses <- function(tibble){
  morningResponses <- read.csv("data/mental_surveys/Morning_Survey_220223.csv") %>%
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
    select(
      userId,
      Survey.Name,
      date,
      `What.time.did.you.actually.try.to.go.to.sleep.last.night.`:`Would.you.like.a.member.of.the.research.team.to.reach.out.to.you.because.you.are.feeling.unsafe.` 
    ) %>% 
    # Add "morning" at the end of the column names that are repeated in the morning and in the evening
    rename_with(~paste0(., "morning"), all_of(morning_columns))
    
  eveningResponses <- read.csv("data/mental_surveys/Evening_Survey_220223.csv") %>% 
    rename(userId = User.Id, date = Survey.Started.Date) %>% 
    mutate(date = as.Date(format(dmy(date), "%Y-%m-%d"))) %>% 
    select(
      userId,
      Survey.Name,
      date,
      `How.would.you.gauge.your.energy.levels.in.the.past.day.`:`Would.you.like.a.member.of.the.research.team.to.reach.out.to.you.because.you.are.feeling.unsafe.` 
    ) %>%
    # Add "evening" at the end of the column names that are repeated in the morning and in the evening
    rename_with(~paste0(., "evening"), all_of(evening_columns))
    
  # merged_data <- bind_cols(tibble, morningResponses, eveningResponses)
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