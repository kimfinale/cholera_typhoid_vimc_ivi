#' Vaccinate population for a set of condition

#' @param efficacy Vaccine efficacy as a proportion
#' @param duration_effect Years during which vaccine-induced immunity remains
#' @param coverage A proportion of target population that receives the vaccine
#' @param age_range Target age range
#' @param year Year in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
calculate_vaccine_protected <- function (disease = NULL,
                                         country = NULL,
                                         population = NULL,
                                         year = 2000:2100,
                                         vacc_efficacy = NULL,
                                         vacc_duration_effect = NULL,
                                         vacc_coverage = NULL) {

  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  dis <- tolower(disease)
  rm(disease)
  if (is.null(vacc_efficacy)) {
      ve_5over <- parameters[tolower(disease) == dis & definition == "vaccine efficacy", value]
      ve_u5 <- parameters[tolower(disease) == dis & definition == "vaccine efficacy (< 5 yo)", value]
      if (length(ve_u5) == 1) { # cholera has a separate value for the < 5 and 5+
        vacc_efficacy <- c(rep(ve_u5, 5), rep(ve_5over, 96))
      } else {
        vacc_efficacy <- rep(ve_5over, 101)
      }
  }

  if (is.null(vacc_duration_effect)) {
    vacc_duration_effect <- parameters[tolower(disease) == dis & definition == "duration of vaccine-induced immunity", value]
    if (length(vacc_duration_effect) != 1) {
      stop("Length is not 1. vacc_duration_effect must be uniquely determined")
    }
  }

  country_clean <- clean_country_names(country)
  rm(country)
  ## probability of dying at age is calculated to
  d1 <- p_dying_age %>%
    filter(country == country_clean, year %in% !!year) %>%
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
  ## Given vaccine coverage rate, determine the number of people who become protected
  ## the calculation involves vaccine waning, vaccine efficacy, population size
  id <- 1
  for (i in 1:(nc-1)) {
    # cat("i =", i, "\n")
    vacc_protected <- data.frame(matrix(0, ncol=nc, nrow=nr)) # this will hold the vaccine protected people by the coverage rate in the year i (ie, future distribution due to aging, waning, death, )
    if(!identical(rep(0.0, nc), as.double(vacc_coverage[, i]))){
      vacc_protected[, i] <- vacc_coverage[, i] * vacc_efficacy * population[, i]
      # vaccine-protection decays exponentially
      # N[t,a] = population size in year t and age a, V[t,a] = vaccinated people at age a in year t, mu[t,a] = crude annual death rate for the age a in year t
      # if N[t,a]*(1 - mu[t,a]) >=  N[t+1, a+1], V[t+1,a+1] = V[t,a] * N[t+1,a+1]/N[t,a]
      # that is, population size decreases more than the death rate (eg, because of migration), we assume that vaccinated people have the same chance as the un-vaccinated people for whatever reason for migrating out
      # this may underestimate the reduction of vaccinated people as the reality will be more likely that there will be more out-migration as there will be in-migration.
      # can we treat that as a stochastic effect? Given the death rate can we calculate the confidence interval

      ## vaccine-induced immunity waning and aging
      for (j in (i+1):nc) {
        t <- j - i # year elapsed since vaccination
        frac_waning <- pexp(t, rate = 1 / vacc_duration_effect) # fraction still protected after elapsed year
        vacc_protected[(t+1):nr, j] <-
          vacc_protected[t:(nr-1), j-1] * (1 - frac_waning) * (1 - p_dying[t:(nr-1), j-1])
      }
      names(vacc_protected) <- names(population)
      population <- population - vacc_protected
      vacc_protected_list[[ id ]] <- vacc_protected
      id <- id + 1
    }
  }
  vacc_protected_total <- Reduce('+', vacc_protected_list)

  return(vacc_protected_total)
}

