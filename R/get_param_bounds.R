#' Generate a set of randomly sampled parameter values for a simulation run
#' '
#' The \code{get_param_bounds()} extracts the upper and lower bounds of the parameters
#' @param x Parameter data frame
#' @export
#' @examples
#' get_param_bounds(parameters)
#'
get_param_bounds <- function(x, disease) {
  bounds <- list()
  dis <- disease
  rm(disease)
  bounds$disease <- dis

  param <- x[disease == dis]

  bounds$prob_healthcare <- param[definition == "Probability of infected patients seeking health care", c(lower_bound, upper_bound)]
  bounds$prob_inpatients <- param[definition == "Probability that infected patients are admitted to hospital", c(lower_bound, upper_bound)]
  bounds$case_fatality_care <- param[definition == "case fatality ratio among patients who seek health care", c(lower_bound, upper_bound)]
  bounds$case_fatality_nocare <- param[definition == "case fatality ratio outside the hospital", c(lower_bound, upper_bound)]
  bounds$vacc_efficacy <- param[definition == "vaccine efficacy", c(lower_bound, upper_bound)]
  bounds$vacc_duration <- param[definition == "duration of vaccine-induced immunity (year)", c(lower_bound, upper_bound)]
  bounds$dis_weight_severe <- param[definition == "disability weight for severe illness", c(lower_bound, upper_bound)]
  bounds$dis_weight_moderate <- param[definition == "disability weight for moderate illness", c(lower_bound, upper_bound)]
  bounds$dis_weight_mild <- param[definition == "disability weight for mild illness", c(lower_bound, upper_bound)]
  bounds$dur_illness <- param[definition == "duration of illness", c(lower_bound, upper_bound)]
  bounds$sanitation_RR <- param[definition == "relative risk of infection from at least basic sanition", c(lower_bound, upper_bound)]

  # parameters for the mean and standar deviation of the normal distribution, which is then transformed using inv_logit function
  mort <- setDT(data.frame(country = c("Bangladesh", "Ethiopia", "Cote d'Ivoire", "Lao",
                                       "Zimbabwe", "India", "Nigeria", "Senegal", "Other"),
                           cfr_mean = c(-3.45, -1.99, -3.24, -4.27, -2.59, -2.80, -1.69, -3.41, -3.07),
                           cfr_se = c(0.72*2, 0.44*2, 0.36*2, 0.58*2, 0.25*2, 0.38, 0.28, 0.51, 0.87)))

  hosp <- setDT(data.frame(country = c("Bangladesh", "India", "Kenya", "Pakistan"),
                           frac_hosp_mean = c(-3.72, -2.78, -2.77, -4.21),
                           frac_hosp_se = c(1.62, 0.37, 1.09, 0.58)))

  mort$country <- clean_country_names(mort$country)
  hosp$country <- clean_country_names(hosp$country)

  bounds$mort <- mort
  bounds$hosp <- hosp

  return (bounds)
}
