#' Sets the parameter valuedotective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{set_params_cholera()} returns a list of parameter values for
#' stochstic simulations
#' @param disease Character strings either "Cholera" or "Typhoid"
#' @param nruns Number of simulation runs
#' @param country Vector of country names to simulate
#' @export
#' @examples
#' set_param_cholera("Cholera", 30, c("Afghanistan", "Zimbabwe"))
#'

set_params_cholera <- function(disease,
                               nruns,
                               country,
                               parameter_data = NULL,
                               incidence_rate_data = NULL,
                               case_fatality_ratio_data = NULL){

  # EDIT 23 June 2025
  # scaling factor for the cholera incidence and case fatality ratio (CFR)
  # Only 32.8% [95% CI: 28.1, 37.9] of cholera cases  seek facility-based care on average
  # More details appear in the file, cholera_model_runs_2025.Rmd
  # care_seeking_prop is added


  # EDIT 18 Jan 2024
  # vaccination_overlap
  # this parameter represents the proportion of the vaccine recipients of the
  # first round that received the second dose. This proportion was extracted
  # by the fact that the mean coverage during the first round was 92.1% and the
  # mean coverage who received the two doses was 69.9% [range: 27.5% to 95.3%]
  # Therefore, the proportion of the first-dose recipients who received the
  # dose as well = 69.9/92.1 = 0.7589577
  # lower and upper bound were derived by the fact
  # lower bound = 27.5/92.1 = 0.2985885
  # upper bound = 95.3/92.1 = 1.034745
  # since the upper bound could not exceed 1,
  # we arbitrarily set as the 95.3%, which is somewhat high and appears to be
  # consistent with coverages at the high end


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
  cn_cfr <- cfr[country == cntry]
  cn_ir <- ir[country == cntry]

  if(sum(is.na(c(cn_cfr$upper, cn_cfr$lower, cn_ir$upper, cn_ir$lower))) == 0){
      # sobol design
      p <- pomp::sobol_design(lower=c(ve0=0, dw0=0, dw1=0, cfr=0, ir=0, vo=0, csp=0, rdc=0),
                              upper=c(ve0=1, dw0=1, dw1=1, cfr=1, ir=1, vo=1, csp=1, rdc=1),
                              nseq = nruns)

      # csp = care-seeking proportion; proportion of cases that present themselves at the healthcare facility
      # parameter transformation
      # extract central, lower bound, and upper bounds
      # take the central value as the mean
      # determine variance of the beta distribution as the wider
      # interval between central-lower and central-upper to account for
      # uncertainty sufficiently
      # determine the alpha and beta parameter

      # Care-seeking proportion
      str <- "Prop_Care_Seeking"
      # extract_param_tf can also be used for cholera with no problem
      csp_lb <- extract_param_tf(pars, str, "lower")
      csp_ub <- extract_param_tf(pars, str, "upper")
      csp_center <- extract_param_tf(pars, str, "val")

      mean_csp <- csp_center
      var_csp <- (((csp_ub - csp_lb)/2)/1.96)^2

      beta_csp <- calc_beta_params(mu = mean_csp, var = var_csp)
      csp_tr <- qbeta(p$csp, shape1 = beta_csp$alpha, shape2 = beta_csp$beta)
      params[, "care_seeking_prop"] <- csp_tr


      # Ratio of reported deaths to reported cases given the true CFR
      str <- "Ratio_Death_Case_Reporting"
      # extract_param_tf can also be used for cholera with no problem
      lb <- extract_param_tf(pars, str, "lower")
      ub <- extract_param_tf(pars, str, "upper")
      center <- extract_param_tf(pars, str, "val")

      mean <- center
      var <- (((ub - lb)/2)/1.96)^2

      beta_p <- calc_beta_params(mu = mean, var = var)
      tr <- qbeta(p$rdc, shape1 = beta_p$alpha, shape2 = beta_p$beta)
      params[, "ratio_death_to_case_report"] <- tr

      # vaccine efficacy
      str <- "vaccine efficacy year 0"
      ve0_lb <- extract_param_tf(pars, str, "lower")
      ve0_ub <- extract_param_tf(pars, str, "upper")
      ve0_center <- extract_param_tf(pars, str, "val")

      mean_ve0 <- ve0_center
      var_ve0 <- ((ve0_center - ve0_lb)/1.96)^2

      beta_ve0 <- calc_beta_params(mu = mean_ve0, var = var_ve0)
      # transformed parameter
      ve0_tr <- qbeta(p$ve0, shape1 = beta_ve0$alpha, shape2 = beta_ve0$beta)
      ve_tr <-
        matrix(ve0_tr, ncol = 1) %*% matrix(unlist(ve_by_yr_prop), nrow = 1)

      params[, paste0("vacc_eff_year_", 0:6)] <- ve_tr

      str <- "disability weight for moderate dirrhoeal disease"
      # extract_param_tf can also be used for cholera with no problem
      dw0_lb <- extract_param_tf(pars, str, "lower")
      dw0_ub <- extract_param_tf(pars, str, "upper")
      dw0_center <- extract_param_tf(pars, str, "val")

      mean_dw0 <- dw0_center
      var_dw0 <- (((dw0_ub - dw0_lb)/2)/1.96)^2

      beta_dw0 <- calc_beta_params(mu = mean_dw0, var = var_dw0)
      dw0_tr <- qbeta(p$dw0, shape1 = beta_dw0$alpha, shape2 = beta_dw0$beta)
      params[, "disability_weight_0"] <- dw0_tr

      str <- "disability weight for severe dirrhoeal disease"
      dw1_lb <- extract_param_tf(pars, str, "lower")
      dw1_ub <- extract_param_tf(pars, str, "upper")
      dw1_center <- extract_param_tf(pars, str, "val")

      mean_dw1 <- dw1_center
      var_dw1 <- (((dw1_ub - dw1_lb)/2)/1.96)^2

      beta_dw1 <- calc_beta_params(mu = mean_dw1, var = var_dw1)
      dw1_tr <- qbeta(p$dw1, shape1 = beta_dw1$alpha, shape2 = beta_dw1$beta)
      params[, "disability_weight_1"] <- dw1_tr

      str <- "proportion of moderate dirrhoeal disease"
      pr0 <- extract_param_tf(pars, str, "val")
      str <- "proportion of severe dirrhoeal disease"
      pr1 <- extract_param_tf(pars, str, "val")

      params[, "prob_severity_0"] <- pr0 / (pr0 + pr1) # normalize
      params[, "prob_severity_1"] <- pr1 / (pr0 + pr1)

      # beta_cfr <- calc_beta_params(mu = cn_cfr$mean/100, var = (cn_cfr$se/100)^2)
      # EDIT 23 June 2025
      # updated to updated cfr_data_cholera_2023 from cfr_data_cholera
      # estimate used rather than mean
      beta_cfr <- calc_beta_params(mu = cn_cfr$estimate, var = (cn_cfr$se)^2)

      cfr_tr <- qbeta(p$cfr, shape1 = beta_cfr$alpha, shape2 = beta_cfr$beta)
      params[, "case_fatality_ratio"] <- cfr_tr
      # incidence rate is assumed to be distributed as lognormal distribution
      # relevant parameters already calculated in cholera_final_model_input_prep.Rmd
      ir_tr <- qlnorm(p$ir, meanlog = cn_ir$meanlog, sdlog = cn_ir$sdlog)
      params[, "incidence_rate"] <- ir_tr

      str <- "duration of illness"
      dur <- extract_param_tf(pars, str, "val") # in days
      params[, paste0("illness_duration_", 0:1)] <- dur / 365

      str <- "vaccination overlap"
      vacc_overlap_lb <- extract_param_tf(pars, str, "lower")
      vacc_overlap_ub <- extract_param_tf(pars, str, "upper")
      vacc_overlap_center <- extract_param_tf(pars, str, "val")
      mean_vo <- vacc_overlap_center
      var_vo <- (((vacc_overlap_ub - vacc_overlap_lb)/2)/1.96)^2
      beta_vo <- calc_beta_params(mu = mean_vo, var = var_vo)
      vo_tr <- qbeta(p$vo, shape1 = beta_vo$alpha, shape2 = beta_vo$beta)
      params[, "vaccination_overlap"] <- vo_tr

      # no uncertainty range are are available for discount rate
      discount_rate_val <-
        extract_param_tf(pars, "Discount rate", "val")
      # no uncertainty range are are available for reference year
      reference_year_val <-
        extract_param_tf(pars, "Reference year", "val")
      params[, "discount_rate"] <- discount_rate_val
      params[, "reference_year"] <- reference_year_val

    } else {
      cat("i =", i, ", country =", cntry, "NA's found\n")
    }
  return(params)
}
