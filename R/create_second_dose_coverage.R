#' Create second dose coverage
#' While we have a  a simple function to create a vaccine coverage for each of the age
#' group and year because vaccine coverage data of the VIMC include two
#' parameters: target population and the coverage rate. Actual coverage rate
#' for the modeling is coverage * target population size / total population size
#'
#' @param vaccine_coverage_data vaccine coverage information in the long format
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
create_second_dose_coverage <- function(country = NULL,
                                    population = NULL,
                                    vaccine_coverage_data = NULL,
                                    year = 2000:2100){
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (is.null(population)) {
    stop("population must be provided")
  }
  if (is.null(vaccine_coverage_data)) {
    stop("vaccine_coverage_data must be provided")
  }

  # vaccine_coverage_data$country <- clean_country_names(vaccine_coverage_data$country)
  # country_clean <- clean_country_names(country)
  cntry = country
  rm(country)
  year_start <- year[1]
  # select vaccine cov data for the target country, year, and vaccination type
  vaccine_coverage_data %>%
    filter(country == cntry, year >= year_start) -> dat

  vacc_coverage <-
    data.frame(matrix(0, nrow = nrow(population), ncol = ncol(population)))
  names(vacc_coverage) <- as.character(year)

  if ("routine" %in% dat$activity_type) {
    dat %>% filter(activity_type == "routine") -> dat_routine
    # routine coverage = coverage for the 0 year old for the years
    # specified in the year column
    vacc_coverage[1, as.character(dat_routine$year)] <- dat_routine$coverage
  }
  if ("campaign" %in% dat$activity_type) {
    dat %>% filter(activity_type == "campaign", vaccine = "OCV2") -> dat_cmp
    yr <- as.character(dat_cmp$year)
    for (j in seq_along(yr)) {
      cmp_yr <- dat_cmp[dat_cmp$year == as.integer(yr[j]), ]
      # +1 for the indexing because age starts from zero
      age_index_start <- as.integer(cmp_yr$age_first) + 1
      age_index_stop <- as.integer(cmp_yr$age_last) + 1

      # Vaccine coverage, 0 < covg < 1, could be larger than 1 because 'target'
      # could be larger than the population (which can be reduced through
      # adjusting by risk
      covg <- cmp_yr$coverage * as.double(cmp_yr$target) /
        sum(population[age_index_start:age_index_stop, yr[j]])
      if (covg > 1) {
        messages("vaccine coverage is larger than 1")
      }
      vacc_coverage[age_index_start:age_index_stop, yr[j]] <- min(1, covg)
    }
  }

  return(vacc_coverage)
}
