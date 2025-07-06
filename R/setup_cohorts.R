#' Cohort setup
#'
#' The \code{setup_cohorts()} sets up cohorts using the interpolated population
#' for both sexes
#' @param country The country of interest
#' @param population_data VIMC population data in a wide format
#' @param year A range of years during which cohorts are created
#' @export
#' @import data.table
#' @examples
#' setup_cohorts()
#'
setup_cohorts <- function (country = NULL,
                           year = 2000:2100,
                           population_data = NULL) {
  if(is.null(country)) {
    stop("Country must be provided")
  }
  if(is.null(population_data)) {
    stop("population_data  must be provided")
  }
  # pop <- gavi201910_int_pop_both
  cntry <- country
  rm(country)
  yr <- year
  rm(year)
  population_data$country <- clean_country_names(population_data$country)

  #population is in wide data format
  population_data %>%
    filter(country == cntry) %>%
    select(-c(country, age_from)) %>%
    mutate(across(everything(), as.integer)) -> pop

  names(pop) <- as.character(names(pop))
  return (pop)
}
