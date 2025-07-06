#' Calculate the effective vaccine coverage, which is the proportion of people
#' fully protected (i.e., factoring vaccine efficacy) by vaccine to the overall
#' population size.

#' @param pop Total population
#' @param vacc_recipients vaccine recipients
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)

calc_vacc_cov_year <- function(pop, vacc_recipients){
  if (sum(pop < 0) > 0) {
    stop("Population size must be larger than zero to
         calcuate the vaccine coverage")
  }
  # implicitly assumes uniform mixing across age groups and therefore,
  # overall vaccine covarage aggregated acorss age groups is needed
  pop_yr <- apply(pop, 2, sum) # sum across age groups for each year
  vacc_recipients_yr <- apply(vacc_recipients, 2, sum)
  return(vacc_recipients_yr / pop_yr)
}

