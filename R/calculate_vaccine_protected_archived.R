#' Vaccinate population for a set of condition

#' @param efficacy Vaccine efficacy as a proportion
#' @param duration_effect Years during which vaccine-induced immunity remains
#' @param coverage A proportion of target population that receives the vaccine
#' @param age_range Target age range
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
calculate_vaccine_protected_archived <- function (country = "Pakistan",
                            population,
                            year = 2000:2100,
                            vacc_efficacy = 0.82,
                            vacc_duration_effect = 6,
                            vacc_coverage = 0.85,
                            vacc_age = 1:15,
                            vacc_year = seq(2020, 2030, 3)) {
  # cohort[ , match( yr , names(cohort) ) ]
  if (!(length(vacc_efficacy) == 1 | length(vacc_efficacy) == length(vacc_age))){
    stop('efficacy must have 1 or the same number of elements as the age_range')
  }
  # if (!(length(vacc_coverage) == 1 | length(vacc_coverage) == length(vacc_age))){
  #   stop('coverage must have 1 or the same number of elements as the age_range')
  # }
  # if just one value is provided for efficacy, efficacy is assumed to be the same across age groups
  if (length(vacc_efficacy) == 1){
    efficacy <- rep(0, nrow(population))
    efficacy[vacc_age+1] <- vacc_efficacy
  }
  # if just one value is provided for coverage, coverage is assumed to be the same across age groups
  if (length(vacc_coverage) == 1){
    vc <- vacc_coverage
    vacc_coverage <- data.frame(matrix(0, nrow = nrow(population), ncol = ncol(population)))
    names(vacc_coverage) <- names(population)
    vacc_coverage[vacc_age + 1, as.character(vacc_year)] <- vc
  }

  ## probability of dying at age is calculated to
  d1 <- p_dying_age %>%
    filter(country == !!country, year %in% !!year) %>%
    select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year)
  ## add rows as the age categories are different
  d2 <- d1[ c(1, rep(2, 4), rep(3:(nrow(d1)-1), each=5), rep(nrow(d1), 6)), ]
  d2 <- d2[, -1] # remove age_from column
  # add columns as year categories are different
  p_dying <- d2[, c(rep(seq_len(ncol(d2)), each=5), ncol(d2))]
  names(p_dying) <- as.character(year)

  # number of people who are in the vaccine-protected states
  vacc_protected_list <- list()
  nc <- ncol(population)
  nr <- nrow(population)
  nms <- names(population)
  for (i in 1:length(vacc_year)) {
    year <- vacc_year[i]
    vacc_protected <- data.frame(matrix(0, ncol=nc, nrow=nr))
    names(vacc_protected) <- nms
    vacc_protected[, as.character(year)] <-
      vacc_coverage[, as.character(year)] * vacc_efficacy * population[, as.character(year)]
    # vaccine-protection decays exponentially
    # N[t,a] = population size in year t and age a, V[t,a] = vaccinated people at age a in year t, mu[t,a] = crude annual death rate for the age a in year t
    # if N[t,a]*(1 - mu[t,a]) >=  N[t+1, a+1], V[t+1,a+1] = V[t,a] * N[t+1,a+1]/N[t,a]
    # that is, population size decreases more than the death rate (eg, because of migration), we assume that vaccinated people have the same chance as the un-vaccinated people for whatever reason for migrating out
    # this may underestimate the reduction of vaccinated people as the reality will be more likely that there will be more out-migration as there will be in-migration.
    # can we treat that as a stochastic effect? Given the death rate can we calculate the confidence interval

    ## vaccine-induced immunity waning and aging
    nr <- nrow(vacc_protected)
    for (yr in (year+1):tail(year, 1)) {
      t <- yr - year # year elapsed since vaccination
      frac_waning <- pexp(t, rate = 1 / vacc_duration_effect) # fraction still protected after elapsed year
      vacc_protected[(t+1):nr, as.character(yr)] <-
        vacc_protected[t:(nr-1), as.character(yr-1)] * (1 - frac_waning) * (1 - p_dying[t:(nr-1), as.character(yr-1)])
    }
    population <- population - vacc_protected
    vacc_protected_list[[i]] <- vacc_protected
  }
  vacc_protected_total <- Reduce('+', vacc_protected_list)

  return(vacc_protected_total)
}

