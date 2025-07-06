#' Create parameter templates where columns indicate country, run_id,
#' and parameter names
#'
#' The \code{set_params_central_tf()} creates a output template where columns indicate
#' country, run_id, parameter values, and later output names are added.
#' @param disease Disease of interest - typhoid or cholera
#' @param nruns Number of simulation run
#' @param country Vector of country names to simulate
#' @param parameter_data Parameter file
#' @param incidence_rate_data Overall incidence rate
#' @export
#' @examples
#' set_params_tf(country = "Afghanistan", nruns = 30)
#'
set_params_central_tf <- function(disease = "Typhoid",
                          nruns=1,
                          country,
                          parameter_data = NULL,
                          incidence_rate_data = NULL){

  dis <- disease
  rm(disease)
  param_names <- c("run_id", "disease", "country", "incidence_rate",
                   "case_fatality_ratio_1",
                   "case_fatality_ratio_2",
                   paste0("disability_weight_", 1:4),
                   paste0("illness_duration_", 1:4),
                   paste0("prob_severity_", 1:4),
                   "duration_vacc_protection",
                   "vacc_efficacy",
                   "indirect_vacc_efficacy",
                   "eff_vacc_cov_limit")

  cntry <- country
  params <- data.frame(matrix(NA, nrow = nruns,
                            ncol = length(param_names)))
  names(params) <- param_names
  params$run_id <- 1:nruns
  params$disease <- rep(dis, nruns)
  params$country <- rep(cntry, nruns)

  # cfr <- calc_cfr(country = cntry)
  # cfr_1, cfr_2 (cfr for children and adults, resp.)

  # extract upper and lower bounds for parameters for transformation
  # incidence rate
  incidence_rate_data %>% filter(country == cntry) %>%
    pull(max) -> ir_ub
  incidence_rate_data %>% filter(country == cntry) %>%
    pull(min) -> ir_lb

  ir_center <- (ir_ub + ir_lb)/2
  # duration of illness in years for 4 subgroups
  # > str_dis_duration
  # [1] "Duration of moderate typhoid fever"
  # [2] "Duration of severe typhoid fever"
  # [3] "Duration of severe typhoid fever with gastrointestinal bleeding"
  # [4] "Duration of  typhoid fever with abdominal complications (other than gastrointestinal bleeding)"
  disdur_center <- extract_param_tf(parameter_data, str_dis_duration, "val")

  # disability weight for 4 subgroups
  diswt_center <- extract_param_tf(parameter_data, str_dis_disability, "val")

  # no uncertainty range are are available for prob of disease severity
  disseverity_val <- extract_param_tf(parameter_data, str_dis_severity, "val")

  str <- "duration of vaccine-induced immunity (year)"
  vaccdur_center <- extract_param_tf(parameter_data, str, "val")

  str <- "total vaccine efficacy in Bangladesh"
  vacceff_center <- extract_param_tf(parameter_data, str, "val")

  str <- "total residents in the study area in Bangladesh"
  total_residents <- extract_param_tf(parameter_data, str, "val")

  str <- "residents who are age-eligible for vaccination in the study area in Bangladesh"
  eligible_residents <- extract_param_tf(parameter_data, str, "val")

  str <- "vaccine covearge for the target population in the study area in Bangladesh"
  vacc_cov <- extract_param_tf(parameter_data, str, "val")
  overall_vacc_cov <- vacc_cov * eligible_residents / total_residents


  eff_vc_limit_center <- overall_vacc_cov * vacceff_center

  str <- "indirect vaccine efficacy in Bangladesh"
  ive_center <- extract_param_tf(parameter_data, str, "val")

  cfr_cntry <- case_fatality_ratio_data[case_fatality_ratio_data$country == cntry, ]
  cfr_1_cntry <- cfr_cntry[cfr_cntry$age == "children", ]$mean
  cfr_2_cntry <- cfr_cntry[cfr_cntry$age == "adult mixed", ]$mean


  params[, "incidence_rate"] <- ir_center
  # params[, "case_fatality_ratio"] <- rep(cfr[1], nruns)
  params[, "disability_weight_1"] <- diswt_center[[1]]
  params[, "disability_weight_2"] <- diswt_center[[2]]
  params[, "disability_weight_3"] <- diswt_center[[3]]
  params[, "disability_weight_4"] <- diswt_center[[4]]
  params[, "illness_duration_1"] <- disdur_center[[1]]
  params[, "illness_duration_2"] <- disdur_center[[2]]
  params[, "illness_duration_3"] <- disdur_center[[3]]
  params[, "illness_duration_4"] <- disdur_center[[4]]
  params[, "prob_severity_1"] <- disseverity_val[[1]]
  params[, "prob_severity_2"] <- disseverity_val[[2]]
  params[, "prob_severity_3"] <- disseverity_val[[3]]
  params[, "prob_severity_4"] <- disseverity_val[[4]]
  params[, "duration_vacc_protection"] <- vaccdur_center
  params[, "vacc_efficacy"] <- vacceff_center
  params[, "indirect_vacc_efficacy"] <- ive_center
  params[, "eff_vacc_cov_limit"] <- eff_vc_limit_center
  ## ------------------------------------------------------------
  ## edited on 9 May 2022
  params[, "case_fatality_ratio_1"] <- cfr_1_cntry
  params[, "case_fatality_ratio_2"] <- cfr_2_cntry

  # cfr differs by age
  # case_fatality_ratio <- data.frame(matrix(nrow = 101, ncol = nruns))
  # case_fatality_ratio[, 1:nruns] <- cfr

  ## edit on 9 May 2022
  ## cfr is now modeled as an estimate based on meta-analysis Marchello et al. ()
  # return(list(case_fatality_ratio = case_fatality_ratio, params = params))
  return(list(params = params))
}
