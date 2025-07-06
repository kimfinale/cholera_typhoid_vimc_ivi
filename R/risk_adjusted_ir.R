#' Calculate incidence rates for those with access to protective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param overall_ir Aggregate incidence rate at the country level
#' @param prop_sanitation Proportion of population with access to at least
#' basic sanitation in the year when the overall incidence rate was estimated
#' @param sanitation_RR Relative risk of those who have access to at least
#' basic sanitation (i.e., < 1)
#' @export
#' @examples
#' ir_sanitation_yesno(overall_ir=150, prop_sanitation=0.5, sanitation_RR=0.5)
#' 100 vs 200 (without access to sanitation)

ir_sanitation_yesno <- function (overall_ir, prop_sanitation, sanitation_RR) {

  ir_san_no <- (overall_ir/(prop_sanitation*sanitation_RR + (1-prop_sanitation)))
  ir_san_yes <- ir_san_no * sanitation_RR

  return (c(ir_san_yes = ir_san_yes, ir_san_no = ir_san_no))
}
