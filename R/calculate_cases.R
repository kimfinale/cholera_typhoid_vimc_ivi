#' Compute the number of cases for a given population and incidence rate
#'
#' The \code{calculate_cases()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate
#' @param disease The disease of interest: cholera or typhoid in lowercase letters
#' @param country The name of the target country
#' @param population A population at risk
#' @param year The range of years during which calculation is conducted. VIMC default is 2000:2100
#' @param ir Incidence rate. If not provided, this is automatically calculated based on the default database and the given country name
#' @export
#' @import data.table
#' @examples
#' pop <- setup_population(disease, country); calculate_cases(disease, country, pop)
calculate_cases <- function (disease = NULL,
                             country = NULL,
                             population = NULL,
                             year = 2000:2100,
                             ir = NULL){

  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  cntry <- clean_country_names(country)
  rm(country)

  dis <- disease
  rm(disease)

  if (is.null(population)) {
    population <- setup_cohorts(country = cntry, year = year)
  }

  if (is.null(ir)) {
    ir <- incidence_rate %>%
    dplyr::filter(tolower(disease) == tolower(dis), tolower(country) == tolower(cntry)) %>%
    pull(incidence_rate_100Kpyo)
  }

  burden <- data.frame(matrix(NA, nrow = nrow(population), ncol = ncol(population)))
  names(burden) <- names(population)

  for (i in 1:ncol(burden)) {
    burden[, i] <- population[, i] * (ir / 1e5)
  }

  return (burden)
}
