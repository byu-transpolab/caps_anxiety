#' Prepare Models Data
#'
#' This function prepares the input data for modeling by performing several preprocessing steps.
#'
#' @param final_table A tibble containing the raw data for modeling.
#'
#' @return A preprocessed data frame ready for modeling.

prep_models <- function(data) {
  data_models <- data %>% 
    mutate(sex = if(is.ordered(sex)) sex else relevel(as.factor(sex), ref = "Male"),
           fsiq_2 = as.numeric(fsiq_2),
           energy = as.numeric(energy_day_q1, na.rm = TRUE),
           motivation = as.numeric(motivation_day_q2, na.rm = TRUE),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group")) %>%
    select(-algorithm) %>% 
    as.data.frame()
}


#' Estimate Models for Suicidal Ideation
#'
#' @param clean_comp_table A tibble containing the data for analysis.
#' @return A list of summaries for each fitted model.
#'
#' @details The function fits several logistic regression models to predict 
#' suicidal ideation. It returns a summary for each model
#'
# Function to create all of the models
estimate_models <- function(data) {
  
  glm1 <- glm(suicidal_ideation_q31_even ~ initial_group, data = data, family = "binomial")
  
  glm2 <- glm(suicidal_ideation_q31_even ~ prescribed_group, data = data, family = "binomial")
  
  glm3 <- glm(suicidal_ideation_q31_even ~ gender, data = data, family = "binomial")
  
  glm4 <- glm(suicidal_ideation_q31_even ~ race, data = data, family = "binomial")
  
  glm5 <- glm(suicidal_ideation_q31_even ~ initial_group + gender + race, data = data, family = "binomial")
  
  glm6 <- update(glm5, formula = .~.+numTrips)
  
  summaries <- list(summary(glm1), summary(glm2), summary(glm3), summary(glm4), summary(glm5), summary(glm6))
  
  return(summaries)
}



