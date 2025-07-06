#' Run models to compute cohort size, cases, deaths, dalys
#'
#' The \code{vaccine_impact_tf()} rules
#' @param disease Target disease (i.e., cholera or typhoid)
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- stoch_run_scenario(disease = Typhoid, country = target_countries, vacc_scenario = "campaign");
vaccine_impact_tf <- function(disease = NULL,
                              country = NULL,
                              population_data = NULL,
                              cohort = NULL,
                              vaccine_coverage_data = NULL,
                              vacc_scenario = NULL,
                              vaccine_coverage = NULL,
                              year = 2000:2100,
                              life_expectancy = NULL,
                              parameter_sample = NULL) {


  # vacc_scenario <- match.arg(vacc_scenario, c("novacc", "routine", "campaign"))

  vacc_scenario <- match.arg(vacc_scenario, c("novacc",
                                              "campaign_default",
                                              "campaign_routine_default",
                                              "campaign_bluesky",
                                              "campaign_routine_bluesky"))
  if (is.null(vacc_scenario)) {
    stop("vacc_scenrio must be provided")
  }
  if (!tolower(vacc_scenario) %in% c("novacc",
                                     "campaign_default",
                                     "campaign_routine_default",
                                     "campaign_bluesky",
                                     "campaign_routine_bluesky")){
    stop("vacc_scenrio must be one of the following: novacc, routine, or campaign")
  }
  if (is.null(disease)) {
    stop("Disease name must be provided: cholera or typhoid")
  }
  # cat( "disease =", disease, ", vacc scenario =", vacc_scenario, "\n")

  # create some tmp variables for clarity
  dis <- disease
  rm(disease)
  cntry <- country
  rm(country)
  yr <- year
  rm(year)

  output <- vector("list") #
  # set up a cohort
  # pop <- setup_cohorts(country = cntry,
  #                      year = yr,
  #                      population_data = population_data)

  # added on 2023-12-26
  pop <- as.data.frame(cohort)
  output$cohort_size <- pop # cohort size is assumed to be the total population
  pop_unprotected <- pop # pop_unprotected is updated under a vaccination scenario
  if (tolower(vacc_scenario) %in% c("campaign_default",
                                    "campaign_routine_default",
                                    "campaign_bluesky",
                                    "campaign_routine_bluesky")) {
    vacc_cov <-
      create_vaccine_coverage(country = cntry,
                              population = pop,
                              vaccine_coverage_data = vaccine_coverage_data)

    # protection for the vaccine recipients
    vacc_protected <-
      calculate_vaccine_protected(
        disease = dis,
        country = cntry,
        year = yr,
        population = pop,
        vaccine_coverage = vacc_cov,
        vaccine_efficacy = parameter_sample$vacc_efficacy,
        vaccine_immunity_duration = parameter_sample$duration_vacc_protection,
        exponential_decay = TRUE,
        vaccine_efficacy_by_year = NULL)

    # vaccine recipients needed to account for unvaccinated people
    # who may be protected indirectly
    vacc_recipients <-
      calculate_vaccine_protected(disease = dis,
                                  country = cntry,
                                  population = pop,
                                  year = yr,
                                  vaccine_coverage = vacc_cov,
                                  vaccine_efficacy = 1.0,
                                  vaccine_immunity_duration = 1000,
                                  exponential_decay = TRUE,
                                  vaccine_efficacy_by_year = NULL)

    # factor protection for vaccine recipients
    pop_unprotected <- pop - vacc_protected #
    # effective vaccine coverage used to switch on/off indirect effect
    eff_vacc_cov_year <-
      calc_vacc_cov_year(pop = pop, vacc_recipients = vacc_protected)

    pop_unvacc <- pop - vacc_recipients
    indirect_vacc_protected <-
      calc_indirect_vacc_protected(
        disease = dis,
        country = cntry,
        population = pop_unvacc,
        vacc_cov = eff_vacc_cov_year,
        vacc_cov_limit = parameter_sample$eff_vacc_cov_limit,
        indirect_vacc_efficacy = parameter_sample$indirect_vacc_efficacy)
    # factor protection for unvaccinated population via indirect protection
    pop_unprotected <- pop_unprotected - indirect_vacc_protected
  }

  ir_adj <- calc_incid_rate_risk_adj(country = cntry,
                                     overall_ir = parameter_sample$incidence_rate,
                                     wash_risk_ratio = wash_risk_ratio,
                                     wash_prop = wash_prop,
                                     ref_year = 2017,
                                     incid = incid_tf_ihme)

  output$cases <- calculate_cases_tf(country = cntry,
                                     population = pop_unprotected,
                                     year = yr,
                                     incidence_rate = ir_adj)

  # June 10, 2024
  # Case fatality ratio may need to be applied to the hospitalized patients
  #
  output$deaths <-
    as.data.frame(
      lapply(1:ncol(output$cases),
           function(x) output$cases[, x] * parameter_sample$case_fatality_ratio))

  # edited on 26 Dec 2023
  # cfr = (parameter_sample$case_fatality_ratio_1 +
  #          parameter_sample$case_fatality_ratio_2)/2
  #
  # output$deaths <-
  #   as.data.frame(
  #     lapply(1:ncol(output$cases),
  #            function(x) output$cases[, x] * cfr))

  names(output$deaths) <- names(output$cases)


  # comment 2023-12-26
  # output$dalys <- calculate_DALY_tf(country = cntry,
  #                                   cases = output$cases,
  #                                   year = yr,
  #                                   life_expectancy = life_expectancy,
  #                                   disability_weight = parameter_sample$disability_weight_1,
  #                                   prob_severity = parameter_sample$prob_severity_1,
  #                                   illness_duration = parameter_sample$illness_duration_1,
  #                                   case_fatality = parameter_sample$case_fatality_ratio_1)


  # country = cntry
  # cases = output$cases
  # year = yr
  # life_expectancy = life_expectancy
  # disability_weight = parameter_sample$disability_weight
  # prob_severity = parameter_sample$prob_severity
  # illness_duration = parameter_sample$illness_duration
  # case_fatality = parameter_sample$case_fatality_ratio
  # discount_rate = parameter_sample$discount_rate

  daly_yll <- calculate_DALY_tf(country = cntry,
                                cases = output$cases,
                                year = yr,
                                life_expectancy = life_expectancy,
                                disability_weight = parameter_sample$disability_weight,
                                prob_severity = parameter_sample$prob_severity,
                                illness_duration = parameter_sample$illness_duration,
                                case_fatality = parameter_sample$case_fatality_ratio,
                                discount_rate = parameter_sample$discount_rate,
                                reference_year = parameter_sample$reference_year)

  # edited for the 2023 runs
  output$dalys <- daly_yll$DALY
  output$yll <- daly_yll$YLL

  return (output)
}
