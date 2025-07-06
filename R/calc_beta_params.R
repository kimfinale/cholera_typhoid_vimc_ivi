#' Calculate alpha and beta parameters of the Beta distribution for a given
#' mean and variance
#'
#' The \code{calc_beta_params()} computes YLD.
#' @param mu Mean
#' @param var Variance
#' @export
#' @examples
#' calc_beta_params(mu = 0.5, var = 0.01)
#'

calc_beta_params <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
