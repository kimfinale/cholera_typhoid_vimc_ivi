#' Compute the number of cases for a given population and incidence rate
#'
#' The \code{calculate_cases()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate. \code{calculate_cases_tf()} is
#' the same, but this function is created just for clarity of using cholera
#' @param country The name of the target country
#' @param population A population at risk
#' @param year The range of years during which calculation is conducted. VIMC default is 2000:2100
#' year, incidence rates for both groups are calculated
#' @param incidence_rate Incidence rate. If not provided, this is automatically calculated based on the default database and the given country name
#' @export
#' @import data.table
#' @examples
calculate_cases <- function (country = NULL,
                             population = NULL,
                             year = 2000:2100,
                             incidence_rate = NULL) {


  if (is.null(country)) {
    stop("Country name must be provided")
  }
  cntry <- clean_country_names(country)
  rm(country)

  if (is.null(population)) {
    # population <- setup_cohorts(country = cntry, year = year)
    stop("Population must be provided")
  }

  incid <-
    data.frame(matrix(NA, nrow = nrow(population), ncol = ncol(population)))
  names(incid) <- names(population)

  for (i in 1:ncol(incid)) {
    incid[, i] <- population[, i] * incidence_rate[, i] / 1e5
  }

  return (incid)
}

