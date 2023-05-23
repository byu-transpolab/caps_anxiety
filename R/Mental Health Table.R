#' @param accurate_tibble target
#' @return a table including ID, date, sf, algorithm, and the 
#' number of clusters as its own column

addNumTrips <- function(df){
  tibble <- as_tibble(df) %>%
    select(id, date, cleaned, algorithm) %>%
    group_by(id,date) %>%
    rowwise() %>%
    # Calculate the total number of trips someone made in a day
    mutate(numTrips = length(algorithm[[1]]))
  return(tibble)
}

#' @param output from addNumTrips function
#' @return a table including ID, date, sf, algorithm, the
#' number of total clusters as its own column, and how many of those
#'  clusters were at a park, library, or grocery store

addTripType <- function(tibble){
  
  parksSf <- sf::st_read("resources/parks.geojson") %>%
    st_transform(32612)
  grocerySf <- sf::st_read("resources/groceries.geojson") %>%
    st_transform(32612)
  librarySf <- sf::st_read("resources/libraries.geojson") %>%
    st_transform(32612)
  
  tibble %>%
    filter(!is.character(algorithm)) %>%
    select(-c(cleaned)) %>%
    unnest(cols = c(algorithm)) %>%
    st_as_sf() %>%
    st_join(parksSf) %>%
    st_join(grocerySf) %>%
    st_join(librarySf) %>%
    mutate(id = id.x)
    
}
