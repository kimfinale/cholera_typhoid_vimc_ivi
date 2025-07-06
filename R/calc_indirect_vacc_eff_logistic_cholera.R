#' Calculates indirect vaccine efficacy (proportion) of cholera
#' for a given vaccine coverage
#'
#' The \code{calc_indirect_eff_logistic_cholera()} computes the indirect vaccine
#' efficacy as a proportional reduction.
#' @param x Effective vaccine coverage, vaccine coverage multiplied by
#' vaccine efficacy, which may vary by year since vaccination
#' @export
#' @examples
#' See cholera_model_input.Rmd for parameter values.  Logistic functions were
#' fit using SSlogis function
calc_indirect_vacc_eff_logistic_cholera <-
  function(eff_vacc_cov = NULL,
          asym = 0.9407624,
          xmid = 0.2698561,
          scal = 0.07744146,
          cutoff = 0.1) {

  indirect_vacc_eff <- asym / (1 + exp((xmid - eff_vacc_cov) / scal))
  # Kolkata and Matlab data (Ali 2005, 2013, see cholera_final_input.Rmd)
  # suggest that under this vaccine
  # coverage do not generate indirect vaccine effect
  indirect_vacc_eff[eff_vacc_cov < cutoff] <- 0

  return(indirect_vacc_eff)
}
