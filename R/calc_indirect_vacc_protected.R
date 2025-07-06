#' Calculate the population that is indirectly protected by vaccine

#' @param population Population who are not protected by direct vaccine impact
#' @param vacc_cov A vector of overall vaccine coverage by year
#' @param vacc_cov_limit Lowest vaccine coverage under which there is no indirect
#' vaccine effect
#' @param indirect_vacc_efficacy A proportion of target population that receives the vaccine
#' @param year A window of years in which vaccination is considered and not
#' the actual years in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
calc_indirect_vacc_protected <- function (disease = NULL,
                                         country = NULL,
                                         population = NULL,
                                         year = 2000:2100,
                                         vacc_cov = NULL,
                                         vacc_cov_limit = NULL,
                                         indirect_vacc_efficacy = NULL) {

  # check input variables
  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  dis <- tolower(disease)
  rm(disease)

  # create tmp variables to remove ambiguity of the same variable names
  country_clean <- clean_country_names(country)
  rm(country)
  yr <- year
  rm(year)

  # number of people who are in the vaccine-protected states
  nc <- ncol(population)
  nr <- nrow(population)
  indirect_vacc_protected <- data.frame(matrix(0, ncol = nc, nrow = nr))
  for (i in 1:nc) {
    # cat("i =", i, "\n")
    if (vacc_cov[i] > vacc_cov_limit) {
      indirect_vacc_protected[, i] <- population[, i] * indirect_vacc_efficacy
    }
  }

  return(indirect_vacc_protected)
}

