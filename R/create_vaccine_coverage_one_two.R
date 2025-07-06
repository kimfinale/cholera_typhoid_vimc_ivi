#' Create vaccine coverage for the first and second dose
#' This is a simple function to create a vaccine coverage for each of the age
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
create_vaccine_coverage_one_two <- function(country = NULL,
                                    population = NULL,
                                    vaccine_coverage_data = NULL,
                                    year = 2000:2100,
                                    vacc_overlap=0.8){
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  if (is.null(population)) {
    stop("population must be provided")
  }
  if (is.null(vaccine_coverage_data)) {
    stop("vaccine_coverage_data must be provided")
  }

  cntry = country
  rm(country)
  vo = vacc_overlap
  rm(vacc_overlap)
  year_start <- year[1]
  # select vaccine cov data for the target country, year, and vaccination type
  vaccine_coverage_data %>%
    filter(country == cntry, year >= year_start) -> dat

  vacc_coverage_one <-
    data.frame(matrix(0, nrow = nrow(population), ncol = ncol(population)))
  names(vacc_coverage_one) <- as.character(year)

  vacc_coverage_two <-
    data.frame(matrix(0, nrow = nrow(population), ncol = ncol(population)))
  names(vacc_coverage_two) <- as.character(year)

  dat %>% filter(activity_type == "campaign") -> dat_cmp
  yr <- as.character(dat_cmp$year)

  for (j in seq_along(yr)) {
    cmp_yr <- dat_cmp[dat_cmp$year == as.integer(yr[j]), ]
    # +1 for the indexing because age starts at zero
    age_index_start <- as.integer(cmp_yr$age_first) + 1
    age_index_stop <- as.integer(cmp_yr$age_last) + 1
    # Vaccine coverage, 0 < covg < 1, could be larger than 1 because 'target'
    # could be larger than the population (which can be reduced through
    # adjusting by risk
    cover = cmp_yr$coverage
    # first round has lower coverage than the second
    # this makes the overlapping algorithm, pi, easy.
    cover = cover[order(cover)]
    cover_adj <- c(0, 0) # adjusted coverage
    if (length(cover) > 1) {
      # pi could be any values although the maximum and minimum may be
      # determined by the coverage of the first and the second round of
      # vaccination
      # pi=0.8 is arbitrary, it was selected because it is not possible to set
      # this to be higher for many countries, but data seem to suggest that
      # pi is high (e.g., > 0.7)
      cover_by_dose = ocv_cov_by_dose(vc1=cover[1], vc2=cover[2],
                                      vacc_overlap=vo)
      # convert the one-dose coverage to two-dose coverage by multiplying
      # the ratio of vaccine efficacy (one-dose to two-dose)
      cover_adj[1] <- cover_by_dose$one * as.double(cmp_yr$target[1]) /
      sum(population[age_index_start[1]:age_index_stop[1], yr[j]])

      cover_adj[2] <- cover_by_dose$two * as.double(cmp_yr$target[2]) /
      sum(population[age_index_start[2]:age_index_stop[2], yr[j]])

      if (cover_adj[1] > 1) {
          messages("vaccine coverage for one dose is larger than 1")
      }
      if (cover_adj[2] > 1) {
        messages("vaccine coverage for the two dose is larger than 1")
      }
      vacc_coverage_one[age_index_start[1]:age_index_stop[1], yr[j]] <-
        min(1, cover_adj[1])
      vacc_coverage_two[age_index_start[2]:age_index_stop[2], yr[j]] <-
        min(1, cover_adj[2])

    }
    else {
      cover_adj[1] <- cover * as.double(cmp_yr$target) /
        sum(population[age_index_start:age_index_stop, yr[j]])
      if (cover_adj[1] > 1) {
        messages("vaccine coverage for one dose is larger than 1")
      }
      vacc_coverage_one[age_index_start:age_index_stop, yr[j]] <-
        min(1, cover_adj[1])
    }
    # covg <- cmp_yr$coverage * as.double(cmp_yr$target) /
    #   sum(population[age_index_start:age_index_stop, yr[j]])
  }


  return(list(vacc_coverage_one=vacc_coverage_one,
              vacc_coverage_two=vacc_coverage_two))
}
