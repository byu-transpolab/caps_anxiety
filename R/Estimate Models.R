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
           prescribed_group = fct_recode(prescribed_group, Control = "No Group"),
           area = area/1e6,
           length = length/1000) %>%
    select(-algorithm) %>% 
    as.data.frame()
}


#' Ordinary Least Squares (OLS) Model
#'
#' This function fits an Ordinary Least Squares (OLS) regression model.
#'
#' @param model_data A data frame containing the variables for modeling.
#'
#' @return An object of class "lm" representing the fitted OLS regression model.

ols_model <- function(model_data) {
  follow <- plm(sev_day_follow ~ motivation + sex + age + fsiq_2 + race + prescribed_group, data = model_data, model = "pooling") 
  leadin <- plm(motivation ~ sev_day_leadin + sex + age + fsiq_2 + race + prescribed_group, data = model_data, model = "pooling")

  return(list("follow" = follow,  "leadin" = leadin))
}


#' Estimate Fixed Effects Model
#'
#' This function estimates a fixed effects model using the provided model data.
#'
#' @param model_data A data frame containing the data for modeling.
#'
#' @return A fixed effects model object.

fixed_effects <- function(model_data) {
  follow <- plm(sev_day_follow ~ motivation, index = c("userId", "activityDay"), data = model_data, model = "within")
  leadin <- plm(motivation ~ sev_day_leadin, index = c("userId", "activityDay"), data = model_data, model = "within")


  area <- plm(motivation ~ area, index = c("userId", "activityDay"), data = model_data, model = "within")
  length <- plm(motivation ~ length, index = c("userId", "activityDay"), data = model_data, model = "within")


  return(list("follow" = follow,  "leadin" = leadin, "area" = area, "length" = length))
}


#' Estimate Random Effects Model
#'
#' This function estimates a random effects model using the provided model data.
#'
#' @param model_data A data frame containing the data for modeling.
#'
#' @return A random effects model object.

random_effects <- function(model_data) {
  follow <- plm(sev_day_follow ~ motivation + sex + age + fsiq_2 + race + prescribed_group, index = c("userId", "activityDay"), data = model_data, model = "random")
  leadin <- plm(motivation ~ sev_day_leadin + sex + age + fsiq_2 + race + prescribed_group, index = c("userId", "activityDay"), data = model_data, model = "random")

  return(list("follow" = follow,  "leadin" = leadin))
}


#' Perform Hausman Test
#'
#' This function performs the Hausman test to compare the efficiency of fixed effects 
#' and random effects models.
#'
#' @param fixed A fixed effects model object.
#' @param random A random effects model object.
#'
#' @return A p-value indicating the significance of the Hausman test.
#' 
hausman <- function(fixed, random) {
  leadin <- phtest(fixed$leadin, random$leadin)
  follow <- phtest(fixed$follow, random$follow)
  
  list("leadin" = leadin, "follow" = follow)
}


#' Fixed Effects Analysis
#'
#' This estimates a linear regression model on the 
#' fixed effects values. This is used to examine the relationship between the 
#' intercepts from the fixed effects model and demographic variables.
#'
#' @param fe A fixed effects model object.
#' @param demo A data frame containing demographic information.
#'
#' @return A summary of the linear regression model examining the relationship 
#' between the intercepts from the fixed effects model and demographic variables.
fe_analysis <- function(fe, demo) {
  fixef <- data.frame(userId = names(fixef(fe)), intercept = fixef(fe)) %>% 
    as_tibble()
  
  fe_model <- right_join(demo, fixef, by = "userId") %>% 
    mutate(sex = if(is.ordered(sex)) sex else relevel(as.factor(sex), ref = "Male"),
           fsiq_2 = as.numeric(fsiq_2),
           prescribed_group = fct_recode(prescribed_group, Control = "No Group"))
  
  lm(intercept ~ sex + age + fsiq_2 + prescribed_group, data = fe_model)
}

#' Estimate Fixed Effects Model for groups
#' 
#' @param model_data A data frame containing the data for modeling.
estimate_fe_groups <- function(model_data) {
  model_data |> 
    tibble::tibble() |> 
    dplyr::group_by(prescribed_group) |> 
    tidyr::nest() |> 
    dplyr::mutate(model = purrr::map(data, 
      function(d) plm::plm(sev_day_follow ~ motivation, 
      index = c("userId", "activityDay"), data = d, model = "within")))

}




#
