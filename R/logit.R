#' identify parameters for the beta distribution for a given mean (mu) and
#' variance (var)
#' this works only when mean < var(1-var) otherwise, negative parameter values
#' arise
#' @export
logit <- function(x) {
  stopifnot(x <= 1, x >= 0)
  log(x/(1-x))
}

