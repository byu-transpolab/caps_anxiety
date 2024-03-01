library(zoo)
library(tidyverse)
library(ggplot2)

tar_load(complete_table)
View(complete_table)


selected_userIds <- c(
  "5f46d644765b45216ff25292",
  "5f50c48d4e42d048e9630d16",
  "5f9703888af3740aae5e594e",
  "5f8dd37e116abf3401d7754d", 
  "5f2c9ae2c7467c5d565f5da0", 
  "5f68adba21da9b57edeb308a"
)


filtered_complete_table <- complete_table %>%
  filter(userId %in% selected_userIds)


numTrips_plot <-filtered_complete_table %>%
  ggplot(aes(x = as.Date(activityDay), y = numTrips)) +
  geom_histogram(stat = "identity", color = "black") +
  facet_wrap(~userId, scales = "free_y") +
  labs(
    title = "Distribution of numTrips Over Time for Each User",
    x = "Activity Day",
    y = "numTrips"
  ) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("2020-08-30", "2021-05-30")),
               date_labels = "%b %Y")


relevant_days <- filtered_complete_table %>%
  mutate(numTrips = ifelse(is.na(numTrips), 0, numTrips)) %>%
  group_by(userId) %>%
  mutate(
    sev_day_avg = zoo::rollapply(numTrips, width = 7, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
    sev_day_sum = zoo::rollapply(numTrips, width = 7, partial = TRUE, FUN = sum, align = "left", fill = NA),
    fourteen_day_avg = zoo::rollapply(numTrips, width = 14, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
    fourteen_day_sum = zoo::rollapply(numTrips, width = 14, partial = TRUE, FUN = sum, align = "left", fill = NA),
    thirty_day_avg = zoo::rollapply(numTrips, width = 30, partial = TRUE, FUN = function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm = TRUE)), align = "left", fill = NA),
    thirty_day_sum = zoo::rollapply(numTrips, width = 30, partial = TRUE, FUN = sum, align = "left", fill = NA)
  ) %>%
  ungroup() 


sev_day_avg_plot <- relevant_days %>%
  ggplot(aes(x = as.Date(activityDay), y = sev_day_avg)) +
  geom_histogram(stat = "identity", color = "black") +
  facet_wrap(~userId, scales = "free_y") +
  labs(
    title = "Distribution of sev_day_avg Over Time for Each User",
    x = "Activity Day",
    y = "sev_day_avg"
  ) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("2020-07-30", "2021-09-30")),
               date_labels = "%b %Y")


fourteen_day_avg_plot <- relevant_days %>%
  ggplot(aes(x = as.Date(activityDay), y = fourteen_day_avg)) +
  geom_histogram(stat = "identity", color = "black") +
  facet_wrap(~userId, scales = "free_y") +
  labs(
    title = "Distribution of fourteen_day_avg Over Time for Each User",
    x = "Activity Day",
    y = "fourteen_day_avg"
  ) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("2020-07-30", "2021-09-30")),
               date_labels = "%b %Y")


thirty_day_avg_plot <- relevant_days %>%
  ggplot(aes(x = as.Date(activityDay), y = thirty_day_avg)) +
  geom_histogram(stat = "identity", color = "black") +
  facet_wrap(~userId, scales = "free_y") +
  labs(
    title = "Distribution of thirty_day_avg Over Time for Each User",
    x = "Activity Day",
    y = "thirty_day_avg"
  ) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("2020-07-30", "2021-09-30")),
               date_labels = "%b %Y")



glm1 <- glm(suicidal_ideation_q31_even ~ initial_group, data = relevant_days, family = "binomial")

glm2 <- glm(suicidal_ideation_q31_even ~ prescribed_group, data = relevant_days, family = "binomial")

# glm3 <- glm(suicidal_ideation_q31_even ~ gender, data = relevant_days, family = "binomial")
# 
# glm4 <- glm(suicidal_ideation_q31_even ~ race, data = relevant_days, family = "binomial")
# 
# glm5 <- glm(suicidal_ideation_q31_even ~ initial_group + gender + race, data = relevant_days, family = "binomial")
# 
# glm6 <- update(glm5, formula = .~.+numTrips)
# 
# summaries <- list(summary(glm1), summary(glm2), summary(glm3), summary(glm4), summary(glm5), summary(glm6))



