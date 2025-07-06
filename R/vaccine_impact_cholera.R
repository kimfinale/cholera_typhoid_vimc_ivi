#' Run models to compute cohort size, cases, deaths, dalys
#' The \code{vaccine_impact()} runs les
#' @param disease Target disease (i.e., cholera or typhoid)
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
vaccine_impact_cholera <- function(disease = NULL,
                           country = NULL,
                           population_data = NULL,
                           cohort = NULL,
                           wash_prop = NULL,
                           wash_risk_ratio = NULL,
                           vaccine_coverage_data = NULL,
                           vacc_scenario = NULL,
                           year = NULL,
                           life_expectancy = NULL,
                           vacc_indirect_effect = NULL,
                           parameter_sample = NULL) {


  if (is.null(vacc_scenario)) {
    stop("vacc_scenrio must be provided")
  }
  # vacc_scenarios_cholera is defined in cholera_model_inputs_[0-9]+
  if (!tolower(vacc_scenario) %in% vacc_scenarios_cholera){
    stop("vacc_scenrio must be one of the following: novacc, routine, or campaign")
  }
  if (is.null(disease)) {
    stop("Disease name must be provided: cholera or typhoid")
  }
  #
  # create some temporary variables to prevent confusion
  dis <- disease
  rm(disease)
  cntry <- country
  rm(country)
  yr <- year
  rm(year)
  #
  output <- vector("list") #
  pop <- as.data.frame(cohort)
  output$cohort_size <- pop # cohort size is assumed to be the total population
  pop_unprotected <- pop # pop_unprotected is updated under a vaccination scenario
  pop_unvacc <- pop # pop_unvacc is updated under a vaccination scenario

  # if (tolower(vacc_scenario) %in% c("routine", "campaign")) {
  if (tolower(vacc_scenario) %in% c("campaign_default_ocv1")) {

    vacc_cov <-
      create_vaccine_coverage(country = cntry,
                              population = pop,
                              vaccine_coverage_data = vaccine_coverage_data)
    #
    vacc_eff_by_year <- unlist(parameter_sample[paste0("vacc_eff_year_", 0:6)])
    # convert VE such that it works for the single-dose efficacy
    ve2 <- parameter_data_cholera[definition ==
                                     "vaccine efficacy",]$value
    ve1 <- parameter_data_cholera[definition ==
                                     "vaccine efficacy - single dose",]$value
    vacc_eff_by_year = vacc_eff_by_year*ve1/ve2

    vacc_protected <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov,
                                  vaccine_efficacy = NULL,
                                  vaccine_immunity_duration = NULL,
                                  exponential_decay = FALSE,
                                  vaccine_efficacy_by_year = vacc_eff_by_year)
    # vaccine recipients can be calculated by setting the vaccine efficacy to
    # be 1 and vaccine_efficacy_by_year to be NULL, and duration of the vaccine
    #-derived immunity lasts long (e.g., 1000 years)
    vacc_recipients <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov,
                                  vaccine_efficacy = 1.0,
                                  vaccine_immunity_duration = 1000,
                                  exponential_decay = TRUE,
                                  vaccine_efficacy_by_year = NULL)

    # factor protection for vaccine recipients
    pop_unprotected <- pop_unprotected - vacc_protected
    # vaccine coverage used to switch on/off indirect effect
    # pop_unvacc <- pop_unvacc - vacc_recipients
    # vacc_cov_year <-
    #   calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_recipients)
    eff_vacc_cov_year <-
      calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_protected)

    # EDIT 10 Jan 2024
    # indirect effect is a function of the vaccine coverage of two-dose
    # schedule. However, here the coverage implies a single dose
    # schedule. Thus, the effectie vaccine coverage is adjusted by the
    # ratio of one-dose to two-dose VE
    eff_vacc_cov_year = eff_vacc_cov_year*ve1/ve2

    # indirect_vacc_protected <-
    #   calc_indirect_vacc_protected_cholera(population = pop_unvacc,
    #                                        eff_vacc_cov = eff_vacc_cov_year)
    if (vacc_indirect_effect) {
      indirect_vacc_protected <-
        calc_indirect_vacc_protected_cholera(population = pop_unprotected,
                                             eff_vacc_cov = eff_vacc_cov_year)

      # factor protection for unvaccinated population via indirect protection
      pop_unprotected <- pop_unprotected - indirect_vacc_protected

    }
    pop_unprotected[pop_unprotected < 0] <- 0
  }

  else if (tolower(vacc_scenario) %in% c("campaign_default_ocv12")) {
    # extract the vaccine coverage for one-dose and two-dose, separately
    vo <- parameter_data_cholera[definition ==
                                    "vaccination overlap",]$value
    vacc_cov_one_two = create_vaccine_coverage_one_two(country = cntry,
                              population = pop,
                              vaccine_coverage_data = vaccine_coverage_data,
                              vacc_overlap=vo)

    #
    vacc_eff_by_year <- unlist(parameter_sample[paste0("vacc_eff_year_", 0:6)])
    # convert VE such that it works for the single-dose efficacy
    ve2 <- parameter_data_cholera[definition ==
                                    "vaccine efficacy",]$value
    ve1 <- parameter_data_cholera[definition ==
                                    "vaccine efficacy - single dose",]$value
    vacc_eff_by_year = vacc_eff_by_year*ve1/ve2

    vacc_cov_one = vacc_cov_one_two$vacc_coverage_one

    vacc_protected <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov_one,
                                  vaccine_efficacy = NULL,
                                  vaccine_immunity_duration = NULL,
                                  exponential_decay = FALSE,
                                  vaccine_efficacy_by_year = vacc_eff_by_year)
    # vaccine recipients can be calculated by setting the vaccine efficacy to
    # be 1 and vaccine_efficacy_by_year to be NULL, and duration of the vaccine
    #-derived immunity lasts long (e.g., 1000 years)

    vacc_recipients <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov_one,
                                  vaccine_efficacy = 1.0,
                                  vaccine_immunity_duration = 1000,
                                  exponential_decay = TRUE,
                                  vaccine_efficacy_by_year = NULL)

    # factor protection for vaccine recipients
    pop_unprotected <- pop_unprotected - vacc_protected
    # vaccine coverage used to switch on/off indirect effect
    pop_unvacc <- pop_unvacc - vacc_recipients
    # vacc_cov_year <-
    #   calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_recipients)
    eff_vacc_cov_year <-
      calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_protected)

    # EDIT 10 Jan 2024
    # indirect effect is a function of the vaccine coverage of two-dose
    # schedule. However, here the coverage implies a single dose
    # schedule. Thus, the effectie vaccine coverage is adjusted by the
    # ratio of one-dose to two-dose VE
    eff_vacc_cov_year = eff_vacc_cov_year*ve1/ve2

    if (vacc_indirect_effect) {
      indirect_vacc_protected <-
        calc_indirect_vacc_protected_cholera(population = pop_unprotected,
                                             eff_vacc_cov = eff_vacc_cov_year)

      # factor protection for unvaccinated population via indirect protection
      pop_unprotected <- pop_unprotected - indirect_vacc_protected
    }

    # two-dose recipients
    vacc_cov_two = vacc_cov_one_two$vacc_coverage_two

    vacc_eff_by_year <- unlist(parameter_sample[paste0("vacc_eff_year_", 0:6)])

    vacc_protected <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov_two,
                                  vaccine_efficacy = NULL,
                                  vaccine_immunity_duration = NULL,
                                  exponential_decay = FALSE,
                                  vaccine_efficacy_by_year = vacc_eff_by_year)
    # vaccine recipients can be calculated by setting the vaccine efficacy to
    # be 1 and vaccine_efficacy_by_year to be NULL, and duration of the vaccine
    #-derived immunity lasts long (e.g., 1000 years)
    vacc_recipients <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  vaccine_coverage = vacc_cov_two,
                                  vaccine_efficacy = 1.0,
                                  vaccine_immunity_duration = 1000,
                                  exponential_decay = TRUE,
                                  vaccine_efficacy_by_year = NULL)

    # factor protection for vaccine recipients
    pop_unprotected <- pop_unprotected - vacc_protected
    # vaccine coverage used to switch on/off indirect effect
    pop_unvacc <- pop_unvacc - vacc_recipients
    # vacc_cov_year <-
    #   calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_recipients)
    eff_vacc_cov_year <-
      calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_protected)

    # EDIT 10 Jan 2024
    # indirect effect is a function of the vaccine coverage of two-dose
    # schedule. However, here the coverage implies a single dose
    # schedule. Thus, the effectie vaccine coverage is adjusted by the
    # ratio of one-dose to two-dose VE
    if (vacc_indirect_effect) {
      indirect_vacc_protected <-
        calc_indirect_vacc_protected_cholera(population = pop_unprotected,
                                             eff_vacc_cov = eff_vacc_cov_year)

      # factor protection for unvaccinated population via indirect protection
      pop_unprotected <- pop_unprotected - indirect_vacc_protected
    }
    pop_unprotected[pop_unprotected < 0] <- 0

  }
  # population now only includes those who are not protected through vaccination
  # right now, only vaccine recipients, but can account for indirect events

  inc_rate <- parameter_sample$incidence_rate
  # EDIT 23 June 2025
  # incidence rate is scaled by care seeking proportion.
  # the assumption is that only a fraction of true number of cases was reported
  # to the data that were fed into the model that estimated the incidence rate
  # therefore, we scale up the estimated incidence rate
  inc_rate <- inc_rate / parameter_sample$care_seeking_prop



  ir_adj <- calc_incid_rate_risk_adj_chol(country = cntry,
                                          overall_ir = inc_rate,
                                          wash_risk_ratio = wash_risk_ratio,
                                          wash_prop = wash_prop,
                                          ref_year = 2010)

  output$cases <- calculate_cases(country = cntry,
                                  population = pop_unprotected,
                                  year = yr,
                                  incidence_rate = ir_adj)

  case_fatality <- parameter_sample$case_fatality_ratio

  # EDIT 23 June 2025
  # case_fatality is scaled by ratio of deaths to cases. Note this ratio
  # is to compare how likely deaths are captured in the data compared with
  # cases. An outbreak investigation (Shikanga (2009) AJTMH), for example,
  # active surveillance led to more deaths compared to cases, leading to
  # twice the CFR estimated during passive surveillance.
  # In this case, this ratio would be 0.5 because
  # approximately deaths of deaths were captured compared to the cases during
  # passive surveillance
  case_fatality <- case_fatality / parameter_sample$ratio_death_to_case_report

  output$deaths <- output$cases * case_fatality

  dis_wt <- unlist(parameter_sample[paste0("disability_weight_", 0:1)])
  dur_ill <- unlist(parameter_sample[paste0("illness_duration_", 0:1)])
  prop_sev <- unlist(parameter_sample[paste0("prob_severity_", 0:1)])
  ##

  discount_rate <- unlist(parameter_sample["discount_rate"])
  reference_year <- unlist(parameter_sample["reference_year"])

  # to test `calculate_DALY_cholera` function
  # country = cntry
  # cases = output$cases
  # year = yr
  # life_expectancy = life_expectancy
  # disability_weight = dis_wt
  # prob_severity = prop_sev
  # illness_duration = dur_ill
  # case_fatality = case_fatality
  # discount_rate = discount_rate
  # reference_year = reference_year



  daly_yll<- calculate_DALY_cholera(country = cntry,
                                 cases = output$cases,
                                 year = yr,
                                 life_expectancy = life_expectancy,
                                 disability_weight = dis_wt,
                                 prob_severity = prop_sev,
                                 illness_duration = dur_ill,
                                 case_fatality = case_fatality,
                                 discount_rate = discount_rate,
                                 reference_year = reference_year)

  # edited for the 2023 runs
  output$dalys <- daly_yll$DALY
  output$yll <- daly_yll$YLL

  return (output)
}
