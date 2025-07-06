#' Calculate disbility-adjusted life years (DALY)
#'
#' The \code{calculate_DALY_tf()} is used to calculate DALY and a wrapper of \code{setup_cohorts()}, \code{calculate_cases()}, \code{calculate_YLL()} and \code{calculate_YLD()} functions
#' @param country_name The of country of interest for which you want to set up the population in
#' @param population_data The population data of countries which includes the file
#' @param year A range of years during which
#' @param disability_weight A fraction that expresses the severity of the disease compared to the death
#' @param prob_severity Probability of different severity outcomes
#' @param illness_duration The duration of the disease in terms of days
#' @param life_expectancy Life expectancy at the time of death in years
#' @param case_fatality Fatality among cases as a proportion
#' @export
#' @examples
#' calculate_DALY_tf()

calculate_DALY_tf <- function(country,
                              cases,
                              year = 2000:2100,
                              life_expectancy = NULL,
                              disability_weight = NULL,
                              prob_severity = NULL,
                              illness_duration = NULL,
                              case_fatality = NULL,
                              discount_rate = NULL,
                              reference_year = NULL) {

  # cases = cases
  # country = country
  # year = year
  # life_expectancy = life_expectancy
  # case_fatality = case_fatality
  # discount_rate = discount_rate

  YLL <- calculate_YLL_tf(cases = cases,
                          country = country, #life expectancy varies by country
                          year = year,
                          life_expectancy = life_expectancy,
                          case_fatality = case_fatality,
                          discount_rate = discount_rate)

# cases = cases
# disability_weight = disability_weight
# prob_severity = prob_severity
# illness_duration = illness_duration
# discount_rate = discount_rate

   YLD <- calculate_YLD_tf(cases = cases,
                           disability_weight = disability_weight,
                           prob_severity = prob_severity,
                           illness_duration = illness_duration,
                           discount_rate = discount_rate)

  # discounting according to the reference year
  refyr <- reference_year - 1999 # column for the reference year
  dr <- discount_rate
  for (i in 1:ncol(YLL)) {
    # see Larson (2013) for implementing discounting
    first_yr_dt <- (1/dr)*(1-exp(-dr)) # first year discounted < 1
    # Take the 1 over its value to compute the past
    if (i < refyr) {
      first_yr_dt <- 1/first_yr_dt
    }
    fac <- first_yr_dt * exp(- dr*(i - refyr))  # continous time
    # fac <- 1/(1 + dr)^(i-refyr) # discrete time
    YLL[, i] <- YLL[, i] * fac
    YLD[, i] <- YLD[, i] * fac
  }

  DALY = YLL + YLD
  return (list(YLL=YLL, DALY=DALY))
}
