#' Calculate years lost due to disability (YLD)
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param cases The number of cases with diseases of interest
#' @param disability_weight A fraction that expresses the severity of the disease compared to the death
#' @param illness_duration Duration of illness in terms of year
#' @param prob_severity
#' @export
#' @examples
#' compute_YLD(data.frame(a=100), 0.2, 12/365)

calculate_YLD_cholera <- function (cases = NULL,
                              disability_weight = NULL,
                              prob_severity = NULL,
                              illness_duration = NULL,
                              discount_rate = NULL) {

  if (!((length(disability_weight) == length(prob_severity) &
       length(disability_weight) == length(illness_duration) &
       length(prob_severity) == length(illness_duration)))) {
    stop("disability_weight, prob_severity, and illness_duration must have the same number of elements")
  }

  # YLD <- as.data.frame(
  #   lapply(1:ncol(cases),
  #          function(x) cases[, x] *
  #          sum(prob_severity * disability_weight * illness_duration)))

  # The above equation does not discount for discount rate
  # although (1/dr)*(1-exp(-dr*illness_duration)) has minimal difference from
  # illness_duration because the illness_duration is short

  dr <- discount_rate
  YLD <- as.data.frame(
    lapply(1:ncol(cases),
           function(x) cases[, x] *
             sum(prob_severity * disability_weight * (1/dr)*(1-exp(-dr*illness_duration)))))

  names(YLD) <- names(cases)

  return (YLD)
}
