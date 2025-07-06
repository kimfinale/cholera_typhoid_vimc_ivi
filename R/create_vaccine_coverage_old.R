#' Vaccinate population for a set of condition

#' @param vaccine_coverage_data vaccine coverage information in the long format
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
create_vaccine_coverage_old <- function(disease = NULL,
                                    country = NULL,
                                    population = NULL,
                                    vaccine_coverage_data = NULL,
                                    routine = TRUE,
                                    num_row = 101,
                                    num_col = 101,
                                    year = 2000:2100){
  if (is.null(disease)) {
    stop("Disease name must be provided: cholera or typhoid")
  }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (tolower(disease) == "typhoid") {
    if (is.null(vaccine_coverage_data) & routine & exists("vacc_cov_input_routine_typhoid")){
      vaccine_coverage_data <- vacc_cov_input_routine_typhoid
    }
    else if (is.null(vaccine_coverage_data) & !routine & exists("vacc_cov_input_campaign_typhoid")){
      vaccine_coverage_data <- vacc_cov_input_campaign_typhoid
    }
    else if (is.null(vaccine_coverage_data) & routine & !exists("vacc_cov_input_routine_typhoid")){
      vaccine_coverage_data <- fread("data/coverage_201910gavi-5_typhoid-routine-default-test.csv")
      vaccine_coverage_data$country <- clean_country_names(vaccine_coverage_data$country)
    } ## realize there is another vaccine input (campaign input)
    else if (is.null(vaccine_coverage_data) & !routine & !exists("vacc_cov_input_campaign_typhoid")){
      vaccine_coverage_data <- fread("data/coverage_201910gavi-5_typhoid-campaign-default-test.csv")
      vaccine_coverage_data$country <- clean_country_names(vaccine_coverage_data$country)
    }
  } else if (tolower(disease) == "cholera") {
    if (routine) {
      stop("Only campaign programs are allowed for cholera")
    }
    else if (is.null(vaccine_coverage_data) & !routine & exists("vacc_cov_input_campaign_cholera")){
      vaccine_coverage_data <- vacc_cov_input_campaign_cholera
    }
    else if (is.null(vaccine_coverage_data) & !routine & !exists("vacc_cov_input_campaign_cholera")){
      vaccine_coverage_data <- fread("inst/extdata/coverage_201910gavi-5_cholera-campaign-default.csv")
      vaccine_coverage_data$country <- clean_country_names(vaccine_coverage_data$country)
    }
  }
  vaccine_coverage_data$country <- clean_country_names(vaccine_coverage_data$country)
  country_clean <- clean_country_names(country)
  year_start <- year[1]


  vaccine_coverage_data %>%
    filter(country == country_clean, year >= year_start, activity_type == "campaign" ) -> cmp
  vaccine_coverage_data %>%
    filter(country == country_clean, year >= year_start, activity_type == "routine" ) -> rout
  vacc_coverage <- data.frame(matrix(0, nrow = num_row, ncol = num_col))
  names(vacc_coverage) <- as.character(year)
  if(is.data.frame(rout) & !nrow(rout) == 0){ # campaign vaccine input does not have routine input (i.e., nrow(rout) = 0)
    vacc_coverage[1,] <- rout$coverage ## routine coverage ==  coverage for the 0 year old
  }
  # pop <- setup_cohorts(country = country)
  if(is.data.frame(cmp) & !nrow(cmp) == 0){
    yr <- as.character(cmp$year)
    for(j in seq_along(yr)){
      cmp_yr <- cmp[cmp$year == as.integer(yr[j]), ]
      start <- as.integer(cmp_yr$age_first) + 1 # +1 because age starts from zero
      stop <- as.integer(cmp_yr$age_last) + 1
      # Vaccine coverage may be larger than 1 because target population sizes can be different
      # This led to the negative population size from the calculation: population size - population protected by vaccine
      # Need to figure out how to select multiple row in data.table
      covg <- as.double(cmp_yr$target) * cmp_yr$coverage / sum(population[start:stop, yr[j]])
      vacc_coverage[start:stop, yr[j]] <- min(1, covg)
    }
  }
  return(vacc_coverage)
}
