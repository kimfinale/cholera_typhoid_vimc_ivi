#' Calculates the case fatality ratio using IHME GBD results
#'
#' The \code{calc_cfr()} calcuates case fatality ratio using the IHME GBD results
#' @param country Target country
#' @param ref_year Reference year
#' @export
#' @examples
#' calc_cfr <- function(country = "Afghanistan", ref_year = 2017)
#'
#' this needs to be modified to account for the uncertainty
calc_cfr <- function(country, ref_year = 2017){
  # incid_number_tf_ihme and death_number_tf_ihme are pre-downloaded data sets
  # from IHME GBD results tool at http://ghdx.healthdata.org/gbd-results-tool
  incid_number_tf_ihme %>%
    filter(location_name == country, year == ref_year) -> id
  death_number_tf_ihme %>%
    filter(location_name == country, year == ref_year) -> dd

  # IHME results age category
  age0 <- c(1, seq(5, 90, by = 5))
  age1 <- c(4, seq(9, 94, by = 5))
  age_cat <- c("<1 year", paste0(age0, " to ", age1), "95 plus")

  # Get the age name of the data set in order according to the age category
  # that was just created
  ind <- rep(NA, length(age_cat))
  for(i in 1:length(age_cat)) {
    ind[i] <- which(dd$age_name == age_cat[i])
  }
  cfr <- dd[ind, ]$val / id[ind, ]$val
  # Translate to one year age group (i.e, 0:100) by assuming that cfr remains
  # constant within the age group
  cfr_age <- rep(NA, 101)
  cfr_age[1] <- cfr[which(age_cat == "<1 year")]
  for (ag in age0) {
    # cat("ag =", ag, ", index = ", which(age0 == ag), "\n")
    cfr_age[(ag+1):(ag+5)] <- cfr[which(age0 == ag) + 1]
  }
  cfr_age[96:101] <- cfr[which(age_cat == "95 plus")]

  return(cfr_age)
}

