#' Run models to compute cohort size, cases, deaths, dalys
#' The \code{vaccine_impact()} runs les
#' @param disease Target disease (i.e., cholera or typhoid)
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- stoch_run_scenario(disease = Typhoid, country = target_countries, vacc_scenario = "campaign");
vaccine_impact <- function(disease = NULL,
                           country = NULL,
                           population_data = NULL,
                           wash_prop = NULL,
                           wash_risk_ratio = NULL,
                           vacc_scenario = NULL,
                           vaccine_coverage = NULL,
                           year = NULL,
                           life_expectancy = NULL,
                           incidence_rate = NULL,
                           disability_weight = NULL,
                           case_fatality = NULL,
                           illness_duration = NULL,
                           vaccine_efficacy = NULL,
                           vaccine_immunity_duration = NULL,
                           exponential_decay = FALSE,
                           vaccine_efficacy_by_year = NULL){

  if (is.null(vacc_scenario)) {
    stop("vacc_scenrio must be provided")
  }
  if (!tolower(vacc_scenario) %in% c("novacc", "routine", "campaign")){
    stop("vacc_scenrio must be one of the following: novacc, routine, or campaign")
  }
  if (is.null(disease)) {
    stop("Disease name must be provided: cholera or typhoid")
  }
  # cat( "disease =", disease, ", vacc scenario =", vacc_scenario, "\n")

  # create some temporary variables to prevent confusion other function
  # variables inside
  dis <- disease
  rm(disease)
  cntry <- country
  rm(country)
  yr <- year
  rm(year)

  output <- vector("list") #
  # set up a cohort
  pop <- setup_cohorts(country = cntry,
                       year = yr,
                       population_data = population_data)

  output$cohort_size <- pop # cohort size is assumed to be the total population
  if (tolower(vacc_scenario) == "routine") {
    vacc_cov <-
      create_vaccine_coverage(country = cntry,
                              population = pop,
                              vaccine_coverage_data = vaccine_coverage)
    vacc_protected <-
      calculate_vaccine_protected(disease = dis,
          country = cntry,
          population = pop,
          vaccine_coverage = vacc_cov,
          vaccine_efficacy = vaccine_efficacy,
          vaccine_immunity_duration = vaccine_immunity_duration,
          exponential_decay = exponential_decay,
          vaccine_efficacy_by_year = vaccine_efficacy_by_year)

    pop <- pop - vacc_protected
  }
  else if (tolower(vacc_scenario) == "campaign") {
    # vc <- create_vaccine_coverage(country = cntry, disease = dis, routine = F)
    vacc_cov <-
      create_vaccine_coverage(country = cntry,
                              population = pop,
                              vaccine_coverage_data = vaccine_coverage)
    vacc_protected <-
      calculate_vaccine_protected(disease = dis,
          country = cntry,
          population = pop,
          vaccine_coverage = vacc_cov,
          vaccine_efficacy = vaccine_efficacy,
          vaccine_immunity_duration = vaccine_immunity_duration,
          exponential_decay = exponential_decay,
          vaccine_efficacy_by_year = vaccine_efficacy_by_year)
    pop <- pop - vacc_protected
  }
  # population now only includes those who are not protected through vaccination
  # right now, only vaccine recipients, but can account for indirect events

  ir_adj <- calc_incid_rate_risk_adj_chol(country = cntry,
                                     overall_ir = incidence_rate,
                                     wash_risk_ratio = wash_risk_ratio,
                                     wash_prop = wash_prop,
                                     ref_year = 2010)

  output$cases <- calculate_cases(country = cntry,
                                  population = pop,
                                  year = yr,
                                  incidence_rate = ir_adj)


  output$deaths <- output$cases * case_fatality

  output$dalys <- calculate_DALY(disease = dis,
                                 country = cntry,
                                 cases = output$cases,
                                 year = yr,
                                 life_expectancy = life_expectancy,
                                 disability_weight = disability_weight,
                                 case_fatality = case_fatality,
                                 illness_duration = illness_duration)





  return (output)
}
