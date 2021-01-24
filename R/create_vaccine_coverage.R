#' Vaccinate population for a set of condition

#' @param vacc_input vaccine coverage information in the long format
#' @param country name of the country
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
create_vaccine_coverage <- function(disease = NULL,
                                    country = NULL,
                                    vacc_input = NULL,
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
    if (is.null(vacc_input) & routine & exists("vacc_cov_input_routine_typhoid")){
      vacc_input <- vacc_cov_input_routine_typhoid
    }
    else if (is.null(vacc_input) & !routine & exists("vacc_cov_input_campaign_typhoid")){
      vacc_input <- vacc_cov_input_campaign_typhoid
    }
    else if (is.null(vacc_input) & routine & !exists("vacc_cov_input_routine_typhoid")){
      vacc_input <- fread("data/coverage_201910gavi-5_typhoid-routine-default-test.csv")
      vacc_input$country <- clean_country_names(vacc_input$country)
    } ## realize there is another vaccine input (campaign input)
    else if (is.null(vacc_input) & !routine & !exists("vacc_cov_input_campaign_typhoid")){
      vacc_input <- fread("data/coverage_201910gavi-5_typhoid-campaign-default-test.csv")
      vacc_input$country <- clean_country_names(vacc_input$country)
    }
  } else if (tolower(disease) == "cholera") {
    if (routine) {
      stop("Only campaign programs are allowed for cholera")
    }
    else if (is.null(vacc_input) & !routine & exists("vacc_cov_input_campaign_cholera")){
      vacc_input <- vacc_cov_input_campaign_cholera
    }
    else if (is.null(vacc_input) & !routine & !exists("vacc_cov_input_campaign_cholera")){
      vacc_input <- fread("data/coverage_201910gavi-5_cholera-campaign-default-test.csv")
      vacc_input$country <- clean_country_names(vacc_input$country)
    }
  }

  country_clean <- clean_country_names(country)
  year_start <- year[1]
  vacc_input %>% filter(country == country_clean, year >= year_start, activity_type == "campaign" ) -> cmp
  vacc_input %>% filter(country == country_clean, year >= year_start, activity_type == "routine" ) -> rut
  vacc_coverage <- data.frame(matrix(0, nrow = num_row, ncol = num_col))
  names(vacc_coverage) <- as.character(year)
  if(is.data.frame(rut) & !nrow(rut) == 0){ # campaign vaccine input does not have routine input (i.e., nrow(rut) = 0)
    vacc_coverage[1,] <- rut$coverage ## routine coverage ==  coverage for the 0 year old
  }
  pop <- setup_cohorts(country = country)
  if(is.data.frame(cmp) & !nrow(cmp) == 0){
    yr <- as.character(cmp$year)
    for(j in seq_along(yr)){
      cmp_yr <- cmp[cmp$year == as.integer(yr[j]), ]
      start <- as.integer(cmp_yr$age_first) + 1 # +1 because age starts from zero
      stop <- as.integer(cmp_yr$age_last) + 1
      # Vaccine coverage may be larger than 1 because target population sizes can be different
      # This led to the negative population size from the calculation: population size - population protected by vaccine
      covg <- as.double(cmp_yr$target) * cmp_yr$coverage / sum(pop[start:stop, yr[j]])
      vacc_coverage[start:stop, yr[j]] <- min(1, covg)
    }
  }
  return(vacc_coverage)
}
