#' Compute the number of cases for a given population and incidence rate
#'
#' The \code{reset_param()} is used to calculate estimates number of cases for given interpolated
#' population for both sexes and incidence rate
#' @param new New parameter values
#' @param original Old parameter values
#' @export
#' @import data.table
#' @examples
#' param_revised<- reset(param_population(disease, country); calculate_cases(disease, country, pop)
reset_param <- function (new = NULL, original = NULL) {
  if (is.null(new)) {
    stop("New parameters name must be provided")
  }
  if (is.null(original)) {
    stop("Original parameters must be provided")
  }
  revised <- original
  for (i in 1:nrow(new)) {
    dis <- new[i, disease]
    def <- new[i, definition]
    val <- new[i, value]
    # revised[disease == dis & definition == def, value := val]
    revised[disease == dis & definition == def]$value <- val
  }

  return(revised)
}
