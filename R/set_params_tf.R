#' Create parameter templates where columns indicate country, run_id,
#' and parameter names
#'
#' The \code{set_params_tf()} creates a output template where columns indicate
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
set_params_tf <- function(disease = "Typhoid",
                          nruns,
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
                   "eff_vacc_cov_limit",
                   "discount_rate",
                   "reference_year")

  cntry <- country
  params <- data.frame(matrix(NA, nrow = nruns,
                            ncol = length(param_names)))
  names(params) <- param_names
  params$run_id <- 1:nruns
  params$disease <- rep(dis, nruns)
  params$country <- rep(cntry, nruns)

  # cfr <- calc_cfr(country = cntry)
  # cfr_1, cfr_2 (cfr for children and adults, resp.)

  # sample uniform random number 0 - 1
  # evcl effective vaccine coverage limit for indirect vaccine protection
  p <- pomp::sobol_design(
    lower = c(vaccdur = 0, vacceff = 0, ive=0, evcl=0, wt_1 = 0, wt_2 = 0,
              wt_3 = 0, wt_4 = 0, dur_1 = 0, dur_2 = 0, dur_3 = 0,
              dur_4 = 0, ir = 0, cfr_1 = 0, cfr_2 = 0),
    upper = c(vaccdur = 1, vacceff = 1, ive=1, evcl = 1, wt_1 = 1, wt_2 = 1,
              wt_3 = 1, wt_4 = 1, dur_1 = 1, dur_2 = 1, dur_3 = 1,
              dur_4 = 1, ir = 1, cfr_1 = 1, cfr_2 = 1),
    nseq = nruns)

  # extract upper and lower bounds for parameters for transformation
  # incidence rate
  incidence_rate_data %>% filter(country == cntry) %>%
    pull(max) -> ir_ub
  incidence_rate_data %>% filter(country == cntry) %>%
    pull(min) -> ir_lb

  # duration of illness in years
  disdur_lb <- extract_param_tf(parameter_data, str_dis_duration, "lower")
  disdur_ub <- extract_param_tf(parameter_data, str_dis_duration, "upper")
  disdur_center <- extract_param_tf(parameter_data, str_dis_duration, "val")

  # disability weight
  diswt_lb <- extract_param_tf(parameter_data, str_dis_disability, "lower")
  diswt_ub <- extract_param_tf(parameter_data, str_dis_disability, "upper")
  diswt_center <- extract_param_tf(parameter_data, str_dis_disability, "val")

  # no uncertainty range are are available for prob of disease severity
  disseverity_val <- extract_param_tf(parameter_data, str_dis_severity, "val")
  # no uncertainty range are are available for discount rate
  discount_rate_val <- extract_param_tf(parameter_data, "Discount rate", "val")
  # no uncertainty range are are available for reference year
  reference_year_val <- extract_param_tf(parameter_data, "Reference year", "val")

  str <- "duration of vaccine-induced immunity (year)"
  vaccdur_lb <- extract_param_tf(parameter_data, str, "lower")
  vaccdur_ub <- extract_param_tf(parameter_data, str, "upper")
  vaccdur_center <- extract_param_tf(parameter_data, str, "val")

  str <- "total vaccine efficacy in Bangladesh"
  vacceff_lb <- extract_param_tf(parameter_data, str, "lower")
  vacceff_ub <- extract_param_tf(parameter_data, str, "upper")
  vacceff_center <- extract_param_tf(parameter_data, str, "val")

  str <- "total residents in the study area in Bangladesh"
  total_residents <- extract_param_tf(parameter_data, str, "val")
  str <- "residents who are age-eligible for vaccination in the study area in Bangladesh"
  eligible_residents <- extract_param_tf(parameter_data, str, "val")
  str <- "vaccine covearge for the target population in the study area in Bangladesh"
  vacc_cov <- extract_param_tf(parameter_data, str, "val")
  overall_vacc_cov <- vacc_cov*eligible_residents/total_residents

  eff_vc_limit_lb <- overall_vacc_cov * vacceff_lb
  eff_vc_limit_ub <- overall_vacc_cov * vacceff_ub
  eff_vc_limit_center <- overall_vacc_cov * vacceff_center

  str <- "indirect vaccine efficacy in Bangladesh"
  ive_lb <- extract_param_tf(parameter_data, str, "lower")
  ive_ub <- extract_param_tf(parameter_data, str, "upper")
  ive_center <- extract_param_tf(parameter_data, str, "val")

  # calculate parameters for parameter transformation
  mean_dur <- disdur_center
  # we take the standard error as: (ub - lb)/2/1.96, where ub and lb represent
  # upper and lower bounds of the 95% confidence interval.
  # Altman DG, Bland JM. How to obtain the P value from a confidence interval.
  # BMJ. 2011;343: d2304. doi:10.1136/bmj.d2304

  var_dur <- (((disdur_ub - disdur_lb)/2)/1.96)^2
  mean_wt <- diswt_center
  var_wt <- (((diswt_ub - diswt_lb)/2)/1.96)^2

  # again to factor into uncertainty sufficiently, a wider interval is chosen
  # (mean_ve - vacceff_lb) > (vacceff_ub - mean)
  mean_ve <- vacceff_center
  var_ve <- (((vacceff_ub - vacceff_lb)/2)/1.96)^2

  mean_ive <- ive_center
  var_ive <- (((ive_ub - ive_lb)/2)/1.96)^2

  mean_evcl <- eff_vc_limit_center # effective vaccine coverage limit
  var_evcl <- (((eff_vc_limit_ub - eff_vc_limit_lb)/2)/1.96)^2

  ## calculate the parameters of the Beta distribution
  beta_params_wt <- data.frame(alpha=rep(NA,4), beta=rep(NA,4))
  beta_params_dur <- beta_params_wt
  beta_params_ve <- data.frame(alpha=rep(NA,1), beta=rep(NA,1))
  beta_params_ive <- data.frame(alpha=rep(NA,1), beta=rep(NA,1))
  beta_params_evcl <- data.frame(alpha=rep(NA,1), beta=rep(NA,1))

  for (i in 1:nrow(beta_params_wt)) {
    param_wt <- calc_beta_params(mu = mean_wt[i], var = var_wt[i])
    beta_params_wt$alpha[i] <- param_wt$alpha
    beta_params_wt$beta[i] <- param_wt$beta

    param_dur <- calc_beta_params(mu = mean_dur[i], var = var_dur[i])
    beta_params_dur$alpha[i] <- param_dur$alpha
    beta_params_dur$beta[i] <- param_dur$beta
  }

  param_ve <- calc_beta_params(mu = mean_ve, var = var_ve)
  beta_params_ve$alpha <- param_ve$alpha
  beta_params_ve$beta <- param_ve$beta

  param_ive <- calc_beta_params(mu = mean_ive, var = var_ive)
  beta_params_ive$alpha <- param_ive$alpha
  beta_params_ive$beta <- param_ive$beta

  param_evcl <- calc_beta_params(mu = mean_evcl, var = var_evcl)
  beta_params_evcl$alpha <- param_evcl$alpha
  beta_params_evcl$beta <- param_evcl$beta

  # transformation of variables that range from 1 to 1 according to the pre-
  # defined distributions such as Beta, Uniform with different limits, lognormal
  p_trans <- p # this is not the best approach
  for (j in 1:4) {
    eval(parse(text=paste0("wt <- p$wt_", j)))
    wt_tr <- qbeta(wt, shape1 = beta_params_wt$alpha[j],
                   shape2 = beta_params_wt$beta[j])
    eval(parse(text=paste0("p_trans$wt_", j, " <- wt_tr")))

    eval(parse(text=paste0("dur <- p$dur_", j)))
    dur_tr <- qbeta(wt, shape1 = beta_params_dur$alpha[j],
                    shape2 = beta_params_dur$beta[j])
    eval(parse(text=paste0("p_trans$dur_", j, " <- dur_tr")))
  }

  eval(parse(text=paste0("ve <- p$vacceff")))
  ve_tr <- qbeta(ve, shape1 = beta_params_ve$alpha,
                 shape2 = beta_params_ve$beta)
  eval(parse(text=paste0("p_trans$ve <- ve_tr")))

  eval(parse(text=paste0("ive <- p$ive")))
  ive_tr <- qbeta(ive, shape1 = beta_params_ive$alpha,
                  shape2 = beta_params_ive$beta)
  eval(parse(text=paste0("p_trans$ive <- ive_tr")))

  eval(parse(text=paste0("evcl <- p$evcl")))
  evcl_tr <- qbeta(ive, shape1 = beta_params_evcl$alpha,
                   shape2 = beta_params_evcl$beta)
  eval(parse(text=paste0("p_trans$evcl <- evcl_tr")))

  eval(parse(text=paste0("vd <- p$vaccdur")))
  vd_tr <- qunif(vd, min = vaccdur_lb, max = vaccdur_ub)
  eval(parse(text=paste0("p_trans$vd <- vd_tr")))

  eval(parse(text=paste0("ir <- p$ir")))
  ir_tr <- qunif(ir, min = ir_lb, max = ir_ub)
  eval(parse(text=paste0("p_trans$ir <- ir_tr")))

  ## ------------------------------------------------------------
  ## edited on 9 May 2022
  cfr_cntry <- case_fatality_ratio_data[case_fatality_ratio_data$country == country, ]
  cfr_1_cntry <- cfr_cntry[cfr_cntry$age == "children", ]
  cfr_2_cntry <- cfr_cntry[cfr_cntry$age == "adult mixed", ]
  cfr_1_tr <- qbeta(p$cfr_1, shape1 = cfr_1_cntry$beta_alpha,
                    shape2 = cfr_1_cntry$beta_beta)
  cfr_2_tr <- qbeta(p$cfr_2, shape1 = cfr_2_cntry$beta_alpha,
                    shape2 = cfr_2_cntry$beta_beta)

  p_trans$cfr_1 <- cfr_1_tr
  p_trans$cfr_2 <- cfr_2_tr
  ## ------------------------------------------------------------
  params[, "incidence_rate"] <- p_trans$ir
  # params[, "case_fatality_ratio"] <- rep(cfr[1], nruns)
  params[, "disability_weight_1"] <- p_trans$wt_1
  params[, "disability_weight_2"] <- p_trans$wt_2
  params[, "disability_weight_3"] <- p_trans$wt_3
  params[, "disability_weight_4"] <- p_trans$wt_4
  params[, "illness_duration_1"] <- p_trans$dur_1
  params[, "illness_duration_2"] <- p_trans$dur_2
  params[, "illness_duration_3"] <- p_trans$dur_3
  params[, "illness_duration_4"] <- p_trans$dur_4
  params[, "prob_severity_1"] <- disseverity_val[1]
  params[, "prob_severity_2"] <- disseverity_val[2]
  params[, "prob_severity_3"] <- disseverity_val[3]
  params[, "prob_severity_4"] <- disseverity_val[4]
  params[, "duration_vacc_protection"] <- p_trans$vd
  params[, "vacc_efficacy"] <- p_trans$ve
  params[, "indirect_vacc_efficacy"] <- p_trans$ive
  params[, "eff_vacc_cov_limit"] <- p_trans$evcl
  ## ------------------------------------------------------------
  ## edited on 9 May 2022
  params[, "case_fatality_ratio_1"] <- p_trans$cfr_1
  params[, "case_fatality_ratio_2"] <- p_trans$cfr_2

  # cfr differs by age
  # case_fatality_ratio <- data.frame(matrix(nrow = 101, ncol = nruns))
  # case_fatality_ratio[, 1:nruns] <- cfr

  ## edit on 9 May 2022
  ## cfr is now modeled as an estimate based on meta-analysis Marchello et al. ()
  # return(list(case_fatality_ratio = case_fatality_ratio, params = params))
  params[, "discount_rate"] <- discount_rate_val
  params[, "reference_year"] <- reference_year_val

  return(list(params = params))
}
