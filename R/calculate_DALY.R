#' Calculate disbility-adjusted life years (DALY)
#'
#' The \code{calculate_DALY()} is used to calculate DALY and a wrapper of \code{setup_cohorts()}, \code{calculate_cases()}, \code{calculate_YLL()} and \code{calculate_YLD()} functions
#' @param country_name The of country of interest for which you want to set up the population in
#' @param population_data The population data of countries which includes the file
#' @param year A range of years during which
#' @param disability_weight A fraction that expresses the severity of the disease compared to the death
#' @param illness_duration The duration of the disease in terms of days
#' @param life_expectancy Life expectancy at the time of death in years
#' @export
#' @examples
#' compute_DALY()

calculate_DALY <- function (disease = NULL,
                            country = NULL,
                            cases = NULL,
                            year = 2000:2100,
                            disability_weight = NULL,
                            illness_duration = NULL,
                            case_fatality = NULL,
                            life_expectancy_data = "data/201910gavi-5_dds-201910_2_life_ex_both.csv") {

  YLL <- calculate_YLL(cases = cases, country = country, disease = disease, life_expectancy_data = life_expectancy_data)
  YLD <- calculate_YLD(cases = cases, disease = disease)


  return (YLL + YLD)
}
