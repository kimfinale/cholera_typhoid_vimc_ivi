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
                           case_fatality = NULL,
                           illness_duration = NULL) {

  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  if (is.null(cases)) {
    stop("Cases name must be provided")
  }
  dis <- tolower(disease)
  rm(disease)

  ill_dur <- illness_duration
  dis_wt <- disability_weight
  cfr <- case_fatality

  YLD <-
    as.data.frame(lapply(1:ncol(cases),
                         function(x) cases[, x] * (1-cfr) * dis_wt * ill_dur))
  names(YLD) <- names(cases)

  return (YLD)
}
