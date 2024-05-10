## METHODS
# Descriptive numeric statistics
desc_num <- function(demo_ids) {
  descriptives_num <- demo_ids %>% 
    filter(!is.na(userId)) %>% 
    select(age, fsiq_2) %>% 
    mutate(fsiq_2 = as.numeric(fsiq_2)) %>%
    rename(Age = age,
           IQ = fsiq_2) %>% 
    datasummary_skim()
}

# Descriptive categorical statistics
desc_cat <- function(demo_ids) {
  descriptives_cat <- demo_ids %>% 
    filter(!is.na(userId)) %>% 
    select(prescribed_group, sex, race) %>% 
    mutate(prescribed_group = fct_recode(prescribed_group, Control = "No Group")) %>% 
    rename(Group = prescribed_group,
           Sex = sex,
           Race = race) %>% 
    datasummary_skim(type = "categorical")
}

# Distribution of the number of activities engaged in
numAct <- function(final_table) {
  raw_trips <- final_table %>% 
    filter(!is.na(numTrips))
  
  ggplot(raw_trips, aes(x = numTrips)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    # geom_text(stat = "count", aes(label = after_stat(count)),
              # vjust = -0.5, color = "black", size = 3) +
    labs(title = "Distribution of Number of Activities", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

# Distribution of the 7-day rolling average number of activities
numAct_7 <- function(final_table) {
  trips <- final_table %>% 
    filter(!is.na(sev_day_avg))
  
  ggplot(trips, aes(x = sev_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Seven Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

# Distribution of the 14-day rolling average number of activities
numAct_14 <- function(final_table) {
  trips <- final_table %>% 
    filter(!is.na(fourteen_day_avg))
  
  ggplot(trips, aes(x = fourteen_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Fourteen Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

# Distribution of the 30-day rolling average number of activities
numAct_30 <- function(final_table) {
  trips <- final_table %>% 
    filter(!is.na(thirty_day_avg))
  
  ggplot(trips, aes(x = thirty_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Thirty Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

# Distribution of the area covered as calculated by the convex_hull
area_dist <- function(final_table) {
  trips <- final_table %>% 
    filter(!is.na(area))
  
  ggplot(trips, aes(x = area/(1e6))) +
    geom_histogram(fill = "darkgray", color = "black") +
    scale_x_log10() +
    labs(title = "Distribution of Area Covered", x = "Area (km^2)", y = "Frequency") +
    theme_bw()
}
