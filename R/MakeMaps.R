#' Function to get random IDs and Dates to map
#'
#'
#' @param cleaned_data target
#' @return tibble of 10 random IDs with 3 dates each
#' @details the returned tibble is the IDs and dates that will
#' be mapped in order to get random days and numbers of 
#' clusters
getRandomDates <- function(cleaned_data) {
  myIDs <- unique(cleaned_data$id)
  randomID <- sample(myIDs, size = min(20,length(myIDs)))
  randomDate <- cleaned_data %>% filter(id %in% randomID) %>%
    group_by(id) %>% slice_sample(n = 5)
  return(randomDate)
}

#' Function to clean GPS data
#'
#'
#' @param cleaned_data target
#' @return list of maps, specifically the "maps" target
#' @details makes a map for each random day for each  of the 10 random IDs
makeAllMaps <- function(cleaned_data){
  myDays <- getRandomDates(cleaned_data)
  dir.create("maps20220826", showWarnings = FALSE)
  
  future_lapply(seq_along(1:nrow(myDays)), function(i){
    st_write(
      myDays$cleaned[[i]] %>%
        mutate(time = as.numeric(timestamp)) %>%
        st_transform(4326),  
      str_c( "maps20220826/", myDays$date[i], "_", myDays$id[i], ".geojson", sep = ""),
      delete_dsn = TRUE, delete_layer = TRUE,
    )
  }, future.seed = NULL)
}


