#' Saturating exponential function
#'
#' The \code{saturaing_exp()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate
#' @param params Parameters (n=2) for the saturating exponential function
#' @param x Variable (year)
#' @export
#' @examples
#' saturaing_exp(x, pars)
saturaing_exp <- function (x, params) {
  1 - exp(-params[2] * (x - params[1]))
}

