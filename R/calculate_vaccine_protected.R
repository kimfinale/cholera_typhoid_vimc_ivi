#' Calculate the vaccine-protected population

#' @param vaccine_efficacy Vaccine efficacy as a proportion
#' @param vaccine_coverage A proportion of target population that receives the vaccine
#' @param vaccine_efficacy A proportion of target population that receives the vaccine
#' @param vaccine_immunity_duration Years during which vaccine-induced immunity remains
#' @param year A window of years in which vaccination is considered and not
#' the actual years in which vaccination is implemented
#' @export
#' @examples
#' pop <- setup_population(); compute_cases(pop)
calculate_vaccine_protected <- function (disease = NULL,
                                         country = NULL,
                                         population = NULL,
                                         year = 2000:2100,
                                         vaccine_coverage = NULL,
                                         vaccine_efficacy = NULL,
                                         vaccine_immunity_duration = NULL,
                                         exponential_decay = TRUE,
                                         vaccine_efficacy_by_year = NULL) {

  # check input variables
  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(country)) {
    stop("Country name must be provided")
  }
  dis <- tolower(disease)
  rm(disease)


  # if (is.null(vaccine_efficacy)) {
  #     ve_5over <- parameters[tolower(disease) == dis & definition == "vaccine efficacy", value]
  #     ve_u5 <- parameters[tolower(disease) == dis & definition == "vaccine efficacy (< 5 yo)", value]
  #     if (length(ve_u5) == 1) { # cholera has a separate value for the < 5 and 5+
  #       vaccine_efficacy <- c(rep(ve_u5, 5), rep(ve_5over, 96))
  #     } else {
  #       vaccine_efficacy <- rep(ve_5over, 101)
  #     }
  # }
  #
  # if (is.null(vaccine_immunity_duration)) {
  #   vaccine_immunity_duration <-
  #     parameters[tolower(disease) == dis &
  #                  definition == "duration of vaccine-induced immunity", value]
  #   if (length(vaccine_immunity_duration) != 1) {
  #     stop("Length is not 1. vaccine_immunity_duration must be uniquely determined")
  #   }
  # }

  # create tmp variables to remove ambiguity of the same variable names
  cntry <- country
  rm(country)
  yr <- year
  rm(year)

  # prob_dying_age is an internal variable of this package
  # indicates the probability of dying at a certain age
  # ---------------------------------------------------------------------
#
  # prob_dying_age %>%
  #   dplyr::filter(country == cntry, year %in% yr) %>%
  #   dplyr::select(age_from, year, value) %>%
  #   tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year) -> d1

# The above code generated the following error
# Error in `tidyr::pivot_wider()`:
# ! `id_cols` can't select a column already selected by
#   `names_from`.
# ℹ Column `year` has already been selected.
# so I changed it into the following
  d1 <- prob_dying_age %>%
    dplyr::filter(country == cntry, year %in% yr) %>%
    dplyr::select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols=c(age_from), names_from=year, values_from=value)
  # -----------------------------------------------------------------
  # add rows as the age categories are wider for prob_dying_age (e.g., 5 years)
  # compared to the population data in which the age category is 1 year
  # the job is to make from 0, 1, 5, 10, ..., 95 to 0:1:100
  # assume that the probability of dying is the same across 1-4 yo, 5-9 yo, and
  # so on.
  d2 <- d1[c(1, rep(2, 4), rep(3:(nrow(d1)-1), each = 5), rep(nrow(d1), 6)), ]
  d2 <- d2[, -1] # remove age_from column
  # add columns as year categories are different
  p_dying <- d2[, c(rep(seq_len(ncol(d2)), each=5), ncol(d2))]
  names(p_dying) <- as.character(yr)

  # number of people who are in the vaccine-protected states
  vacc_protected_list <- list()
  nc <- ncol(population)
  nr <- nrow(population)
  # Given vaccine coverage rate, determine the number of people who become
  # protected the calculation accounts for vaccine waning, vaccine efficacy
  # population size
  vacc_year <- 1
  for (i in 1:(nc-1)) {
    # cat("i =", i, "\n")
    # this will hold the vaccine protected people by the coverage rate
    # in the year i (ie, future distribution due to aging, waning, death)
    vacc_protected <- data.frame(matrix(0, ncol = nc, nrow = nr))
    if (sum(vaccine_coverage[, i]) > 0 & sum(vaccine_coverage[, i] < 0) == 0) {
      if (exponential_decay) {
        # all-or-nothing vaccine: effective vaccination coverage is defined
        # as the vaccine coverage multiplied by the vaccine efficacy. Vaccine
        # recipients are fully protected by being vaccinated at year i
        vacc_protected[, i] <-
          vaccine_coverage[, i] * vaccine_efficacy * population[, i]
        # vaccine-induced immunity waning and aging
        for (j in (i+1):nc) {
          t <- j - i # year elapsed since vaccination
          # fraction that  protected after elapsed year
          ## arbitrary cutoff, 100 years, when one wants to ignore waning
          long_period <- 100 #years
          if (vaccine_immunity_duration > long_period){
            frac_waning <- 0
          } else {
            frac_waning <- pexp(t, rate = 1 / vaccine_immunity_duration)
          }

          vacc_protected[(t+1):nr, j] <-
            vacc_protected[t:(nr-1), j-1] * (1 - frac_waning) * (1 - p_dying[t:(nr-1), j-1])
        }
      }
      else if (!is.null(vaccine_efficacy_by_year)) { # cholera

        vacc_protected[, i] <-
          vaccine_coverage[, i] * vaccine_efficacy_by_year[1] * population[, i]
        for (j in (i+1):nc) {
          t <- j - i # year elapsed since vaccination
          ve <- 0
          # final vaccine efficacy
          if ((t+1) > length(vaccine_efficacy_by_year)) {
            # ve <- as.numeric(tail(vaccine_efficacy_by_year, 1))
            # years for which we don't have a data point, we assume the values
            # for the last data point we have
            nn <- length(vaccine_efficacy_by_year)
            ve <- vaccine_efficacy_by_year[nn] / vaccine_efficacy_by_year[nn-1]
          } else {
            ## relative decrease of vaccine efficacy
            ve <-
              vaccine_efficacy_by_year[ceiling(t+1)] / vaccine_efficacy_by_year[ceiling(t)]
          }
          vacc_protected[(t+1):nr, j] <-
            vacc_protected[t:(nr-1), j-1] * ve * (1 - p_dying[t:(nr-1), j-1])
        }
      }
    }
    names(vacc_protected) <- names(population)
    vacc_protected_list[[i]] <- vacc_protected
  }
  vacc_protected_total <- Reduce('+', vacc_protected_list)

  ## If vaccination is given as high coverage successively
  ## It becomes possible to have people with vaccine-associated protection
  ## is larger than the population size. That should be fixed by setting that
  ## every one is protected.
  for(i in 1:nr) {
    for(j in 1:nc) {
      if(vacc_protected_total[i, j] > population[i, j]) {
        # message(paste0("i = ", i, ", j = ", j, " vacc is larger than pop"))
        vacc_protected_total[i, j] <- population[i, j]
      }
    }
  }

  return(vacc_protected_total)
}

