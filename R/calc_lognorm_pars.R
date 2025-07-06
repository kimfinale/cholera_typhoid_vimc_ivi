#' Calculate Lognormal distribution parameters for a given mean
#' and standard deviation
#'
#' The \code{calc_lognorm_pars()} calculates the parameter values (mu and sigma)
#' for a given mean and standard deviation of observations.
#' @param mean The mean of the observation
#' @param sd The standard deviation of the observations
#' @export
#' @examples
#' param <- calc_lognorm_pars(10, 15)
# x <- rlnorm(1e5, meanlog = param$mu, sdlog = param$sigma)
calc_lognorm_pars <- function(mean, sd){
  v <- sd*sd
  m <- mean
  phi <- sqrt(v + m*m);
  mu <- log(m*m/phi);                # mean of log(Y)
  sigma <- sqrt(log(phi*phi/(m*m))); # std dev of log(Y)

  return(list(mu = mu, sigma = sigma))
}


