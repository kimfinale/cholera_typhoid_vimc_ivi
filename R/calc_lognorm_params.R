#' Calculates indirect vaccine efficacy (proportion) of cholera
#' for a given vaccine coverage
#'
#' The \code{calc_indirect_eff_logistic_cholera()} computes the indirect vaccine
#' efficacy as a proportional reduction.
#' @param x Proportion of vaccine coverage rate

#' @export
#' @examples
#' calc_beta_params(mu = 0.5, var = 0.01)
#'
#'
#'
calc_lognorm_params <- function(mean, sd){
  v <- sd*sd
  m <- mean
  phi <- sqrt(v + m*m);
  mu <- log(m*m/phi);                # mean of log(Y)
  sigma <- sqrt(log(phi*phi/(m*m))); # std dev of log(Y)

  return(list(mu = mu, sigma = sigma))
}

