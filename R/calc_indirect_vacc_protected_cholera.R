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
calc_indirect_vacc_protected_cholera <- function (population = NULL,
                                                  eff_vacc_cov = NULL) {

  evc <- eff_vacc_cov
  rm(eff_vacc_cov)
  # number of people who are in the vaccine-protected states
  nc <- ncol(population)
  nr <- nrow(population)
  indirect_vacc_protected <- data.frame(matrix(0, ncol = nc, nrow = nr))
  # eff_vacc_cov are values (between 0 and 1) across year
  indirect_vacc_efficacy <-
    calc_indirect_vacc_eff_logistic_cholera(eff_vacc_cov = evc)
  for (i in 1:nc) {
    indirect_vacc_protected[, i] <-
      population[, i] * indirect_vacc_efficacy[i]
  }

  return(indirect_vacc_protected)
}

