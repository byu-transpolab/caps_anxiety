


numAct <- function(final_table) {
  raw_trips <- final_table %>% 
    filter(!is.na(numTrips))
  
  # Histogram for numTrips with bin width of 10
  ggplot(raw_trips, aes(x = numTrips)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    geom_text(stat = "count", aes(label = after_stat(count)),
              vjust = -0.5, color = "black", size = 3) +
    labs(title = "Distribution of Number of Activities", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

numAct_7 <- function(final_table) {
  # Histogram for sev_day_avg with bin width of 10
  ggplot(final_table, aes(x = sev_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Seven Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

numAct_14 <- function(final_table) {
  # Histogram for fourteen_day_avg with bin width of 10
  ggplot(final_table, aes(x = fourteen_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Fourteen Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}

numAct_30 <- function(final_table) {
  # Histogram for thirty_day_avg with bin width of 10
  ggplot(final_table, aes(x = thirty_day_avg)) +
    geom_histogram(binwidth = 1, fill = "darkgray", color = "black") +
    labs(title = "Number of Activities with Thirty Day Average", x = "Number of Activities", y = "Frequency") +
    theme_bw()
}
