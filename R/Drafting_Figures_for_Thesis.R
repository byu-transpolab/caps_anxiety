library(targets)
library(tidyverse)

tar_load(demographics)
tar_load(demo_ids)
View(demo_ids)

demo <- demo_ids %>% 
  filter(!is.na(userId))

# Age Distribution
ggplot(demo, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Ages",
       x = "Age",
       y = "Number") +
  scale_y_continuous(breaks = seq(0, max(table(demo$age)), by = 2)) +
  theme_bw()

summary(demo$age)

# Distribution of Prescribed Group
ggplot(demo, aes(x = initial_group)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of Prescribed Groups",
       x = "Prescribed Group",
       y = "Frequency") +
  scale_y_continuous(breaks = seq(0, max(table(demo$prescribed_group)), by = 5)) +
  theme_bw()


# Side by side plot showing initial_group and prescribed_group
demo_long <- pivot_longer(demo, cols = c(initial_group, prescribed_group), names_to = "group_type", values_to = "group")

ggplot(demo_long, aes(x = group)) +
  geom_bar(aes(fill = group_type), position = "dodge") +
  geom_text(data = demo_long %>% filter(group_type == "prescribed_group"),
            stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5, hjust = -1) +  # Adding labels for prescribed_group
  geom_text(data = demo_long %>% filter(group_type == "initial_group"),
            stat = "count", aes(label = after_stat(count)), position = position_dodge(width = 0.9), vjust = -0.5, hjust = 2, color = "black") +  # Adding labels for initial_group
  labs(title = "Distribution of Initial and Prescribed Groups",
       x = "Group",
       y = "Frequency") +
  scale_y_continuous(breaks = seq(0, max(table(demo_long$group)), by = 1)) +
  scale_fill_manual(values = c("initial_group" = "darkgray", "prescribed_group" = "lightgray")) +  # Assigning colors
  theme_bw()

# Results from high scoring algorithm --> 11039 were removed, 91.6% of the userID-activityDays were removed
tar_load(preprocessed)
pre_score <- preprocessed %>% 
  group_by(userId) %>% 
  summarize(total_activityDays = n_distinct(activityDay))

sum(pre_score$total_activityDays) # 12051 userID-activityDay combos


pre_score_days <- preprocessed %>% 
  group_by(userId)

ggplot(pre_score_days, aes(x = activityDay)) +
  geom_bar(stat = "count") +
  labs(title = "Frequency of Activity Days",
       x = "Activity Day",
       y = "Frequency")


tar_load(scored_days)
post_score <- scored_days %>% 
  group_by(userId) %>% 
  summarize(total_activityDays = n_distinct(activityDay))

sum(post_score$total_activityDays) # 1012 userID-activityDay combos

post_score_days <- scored_days %>% 
  group_by(userId)

ggplot(post_score_days, aes(x = activityDay)) +
  geom_bar(stat = "count") +
  labs(title = "Frequency of Activity Days",
       x = "Activity Day",
       y = "Frequency")


