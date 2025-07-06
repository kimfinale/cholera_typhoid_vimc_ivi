#' identify parameters for the beta distribution for a given mean (mu) and
#' variance (var)
#' this works only when mean < var(1-var) otherwise, negative parameter values
#' arise
#' @export
est_beta_params <- function(mu, var) {
  alpha <- ((1 - mu)/var - 1/mu) * mu^2
  beta <- alpha * (1/mu - 1)
  return (params = list(alpha = alpha, beta = beta))
}
