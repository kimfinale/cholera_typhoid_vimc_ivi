#' Calculate years lost due to disability (YLD)
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param disease The name of disease of interest. Now it only supports Cholera or Typhoid
#' @param cases The number of cases with diseases of interest
#' @param disability_weight A fraction that expresses the severity of the disease compared to the death
#' @param illness_duration Duration of illness in terms of year
#' @export
#' @examples
#' compute_YLD(data.frame(a=100), 0.2, 12/365)


calculate_YLD <- function (disease = NULL,
                           cases = NULL,
                           disability_weight = NULL,
                           illness_duration = NULL) {


  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(cases)) {
    stop("Cases name must be provided")
  }
  if (is.null(disability_weight)) {
    disability_weight <- parameters[tolower(disease) == tolower(dis) & definition == "disability weight", value]
    if (length(disability_weight) != 1) {
      stop("Length is not 1. disability weight value must be uniquely determined")
    }
  }
  dis <- tolower(disease)
  rm(disease)

  if (is.null(illness_duration)) {
    r <- parameters[tolower(disease) == dis & definition == "duration of illness"]
    if (nrow(r) == 1) {
      illness_duration <- r$value
    } else {
      stop("Number of rows is not 1. duration of illness value must be uniquely determined")
    }
    if (r$unit == "day"){
      illness_duration <- illness_duration / 365
    }
  }

  disability_weight <- rep(disability_weight, nrow(cases)) # assumes that disability_weight remains constant across age groups
  illness_duration <- rep(illness_duration, nrow(cases)) # assumes that duration remains constant across age groups

  YLD <- as.data.frame(lapply(1:ncol(cases), function(x) cases[, x] * disability_weight * illness_duration))
  names(YLD) <- names(cases)

  return (YLD)
}
