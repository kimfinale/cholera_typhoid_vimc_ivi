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
calc_incid_rate_by_risk <- function(country,
                                    overall_ir,
                                    risk_var,
                                    wash_risk_ratio,
                                    wash_prop,
                                    ref_year = 2017) {
  cntry <- country
  rm(country)
  prop_low_risk <-
    wash_prop %>%
    filter(country == cntry, year == ref_year) %>%
    pull(risk_var)

  rr <-
    wash_risk_ratio %>%
    filter(var == risk_var) %>%
    pull(risk_ratio)

  ir_high <- overall_ir / (prop_low_risk * rr + (1 - prop_low_risk))
  ir_low <- ir_high * rr

  return(list(ir_high = ir_high, ir_low = ir_low))
}

