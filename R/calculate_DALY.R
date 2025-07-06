#' Calculate disbility-adjusted life years (DALY)
#'
#' The \code{calculate_DALY()} is used to calculate DALY and a wrapper of \code{setup_cohorts()}, \code{calculate_cases()}, \code{calculate_YLL()} and \code{calculate_YLD()} functions
#' @param country_name The of country of interest for which you want to set up the population in
#' @param population_data The population data of countries which includes the file
#' @param year A range of years during which
#' @param disability_weight A fraction that expresses the severity of the disease compared to the death
#' @param illness_duration The duration of the disease in terms of days
#' @param life_expectancy Life expectancy at the time of death in years
#' @param case_fatality Case fatality ratio
#' @export
#' @examples
#' compute_DALY()

calculate_DALY <- function (disease,
                            country,
                            cases,
                            year = NULL,
                            life_expectancy = NULL,
                            disability_weight = NULL,
                            case_fatality = NULL,
                            illness_duration = NULL) {

  YLL <- calculate_YLL(disease = disease,
                       cases = cases,
                       country = country,
                       year = year,
                       life_expectancy = life_expectancy,
                       case_fatality = case_fatality)


   YLD <- calculate_YLD(disease = disease,
                       cases = cases,
                       disability_weight = disability_weight,
                       case_fatality = case_fatality,
                       illness_duration = illness_duration)


  return (YLL + YLD)
}
