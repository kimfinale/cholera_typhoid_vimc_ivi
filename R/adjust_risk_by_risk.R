#' Calculate incidence rates for those with access to protective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param overall_ir Aggregate incidence rate at the country level
#' @param prop_high_risk Proportion of population at high risk (i.e., open defecation)
#' @param risk_ratio Risk ratio of high risk relative to the low risk population (i.e., > 1)
#' @export
#' @examples
#' adjust_ir_by_risk(overall_ir = 150, prop_high_risk = 0.3, risk_ratio = 3.5)
#'

adjust_ir_by_risk <- function (overall_ir, prop_high_risk, risk_ratio) {

  ir_low <- overall_ir / (prop_high_risk * risk_ratio + (1 - prop_high_risk))
  ir_high <- ir_low * risk_ratio

  return (c(ir_low = ir_low, ir_high = ir_high))
}
