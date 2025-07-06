#' Calculate incidence rates for those with access to protective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param disease Character string indicating a disease (i.e., "Cholera" or "Typhoid")
#' @param nruns Number of simulation run
#' @param country Vector of country names to simulate
#' @export
#' @examples
#' set_param_cholera("Cholera", 30, c("Afghanistan", "Zimbabwe"))
#'

set_params_central_cholera <- function(disease,
                               nruns=1,
                               country,
                               parameter_data = NULL,
                               incidence_rate_data = NULL,
                               case_fatality_ratio_data = NULL){

  # param_names <- c("run_id", "disease", "country",
  #                  "incidence_rate",
  #                  "case_fatality_ratio",
  #                  paste0("disability_weight_", 0:1),
  #                  paste0("illness_duration_", 0:1),
  #                  paste0("prob_severity_", 0:1),
  #                  paste0("vacc_eff_year_", 0:6))


  param_names <- c("run_id", "disease", "country",
                   "incidence_rate",
                   "vaccination_overlap",
                   "case_fatality_ratio",
                   "care_seeking_prop",
                   "ratio_death_to_case_report",
                   paste0("disability_weight_", 0:1),
                   paste0("illness_duration_", 0:1),
                   paste0("prob_severity_", 0:1),
                   paste0("vacc_eff_year_", 0:6),
                   "discount_rate",
                   "reference_year")

  params <- data.frame(matrix(NA, nrow = nruns, ncol = length(param_names)))

  cntry <- country
  rm(country)
  dis <- disease
  rm(disease)

  names(params) <- param_names
  params$run_id <- 1:nruns
  params$disease <- rep(dis, nruns)
  params$country <- rep(cntry, nruns)

  # create local variables for convenience
  ir <- incidence_rate_data
  rm(incidence_rate_data)
  cfr <- case_fatality_ratio_data
  rm(case_fatality_ratio_data)
  pars <- parameter_data
  rm(parameter_data)

  # proportion of vaccine efficacy remains constant while values may change
  ve_by_yr <- pars[grepl("vaccine efficacy year", definition), .(value)]
  ve_by_yr_prop <- ve_by_yr / rep(ve_by_yr[1], length(ve_by_yr))

  # country-specific parameters
  # cn_cfr <- cfr[country == cntry]$mean
  # updated for 2023
  cn_cfr <- cfr[country == cntry]$estimate
  cn_ir <- ir[country == cntry]$ir_sim

  if(sum(is.na(c(cn_cfr, cn_ir))) == 0){

    strs <- paste0("vaccine efficacy year ",
                  as.character(0:6))
    ve_centers <- extract_param_tf(pars, strs, "val")
    params[, paste0("vacc_eff_year_", 0:6)] <- ve_centers
    str <- "disability weight for moderate dirrhoeal disease"
    # extract_param_tf can also be used for cholera with no problem
    dw0_center <- extract_param_tf(pars, str, "val")
    params[, "disability_weight_0"] <- dw0_center
    str <- "disability weight for severe dirrhoeal disease"
    dw1_center <- extract_param_tf(pars, str, "val")
    params[, "disability_weight_1"] <- dw1_center
    str <- "proportion of moderate dirrhoeal disease"
    pr0 <- extract_param_tf(pars, str, "val")
    str <- "proportion of severe dirrhoeal disease"
    pr1 <- extract_param_tf(pars, str, "val")
    params[, "prob_severity_0"] <- pr0 / (pr0 + pr1) # normalize
    params[, "prob_severity_1"] <- pr1 / (pr0 + pr1)
    params[, "case_fatality_ratio"] <- cn_cfr
    params[, "incidence_rate"] <- cn_ir
    str <- "duration of illness"
    dur <- extract_param_tf(pars, str, "val") # in days
    params[, paste0("illness_duration_", 0:1)] <- dur / 365

    str <- "Prop_Care_Seeking"
    center <- extract_param_tf(pars, str, "val")
    params[, "care_seeking_prop"] <- center

    str <- "Ratio_Death_Case_Reporting"
    center <- extract_param_tf(pars, str, "val")
    params[, "ratio_death_to_case_report"] <- center

  }
  else {
    cat("i =", i, ", country =", cntry, "NA's found\n")
  }
  return(params)
}
