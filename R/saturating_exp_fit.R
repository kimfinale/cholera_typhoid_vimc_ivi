#' Compute the sum of squared difference
#'
#' The \code{ssq()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate
#' @param theta Parameters (n=2) for the saturating exponential function
#' @param x Variable (prportion of pupulation with access to at least basic sanitation in our model)
#' @export
#' @examples
#' saturaing_exp(theta, x)

ssq <- function(theta, x){
  val <- x[,2]
  year <- x[,1]
  tau <- theta[1]
  # k <- theta[2]/(1-theta[2])
  k <- theta[2]
  sum((val - (1 - exp(-k * (year - tau))))^2)
}


#' Compute the number of cases for a given population and incidence rate
#'
#' The \code{reset_param()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate
#' @param theta Parameters (n=2) for the saturating exponential function
#' @param x Variable (prportion of pupulation with access to at least basic sanitation in our model)
#' @import optimx
#' @export
#' @examples
#' saturaing_exp(theta, x)
saturating_exp_fit <- function (x,
                                start = c(1980, 1e-2),
                                lower = c(1e-6, 1e-6),
                                upper = c(2000-1e-3, 10),
                                fun = ssq,
                                ntrace = 0) {
  fit <- optimx::optimx(
    par = start,
    fn = fun,
    x = x,
    method = 'nlminb',
    lower = lower,
    upper = upper,
    itnmax = 100,
    control = list(trace = ntrace))

  return (c(fit$p1, fit$p2, as.integer(fit$convcode)))
}
