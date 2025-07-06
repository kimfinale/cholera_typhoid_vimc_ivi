#' Some miscellaneous functions
#' @export
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

