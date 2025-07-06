#' Saturating exponential function
#'
#' The \code{ocv_cov_by_dose()} is used to calculate the coverage by dose.
#' The function returns oneplus-, two-, and one-dose recipients
#' based on the pi that represent the proportion of the first recipients who
#' goes on to received the second dose (i.e., complete two-dose regimen)
#' population for both sexes and incidence rate
#' @param vc1 Coverage during the first round of vaccination
#' @param vc2 Coverage during the second round of vaccination
#' @param pi Proportion of the first recipients who goes on to received
#' the second dose
#' @export
#' @examples
#' ocv_cov_by_dose(vc1=0.95, vc2=0.95, pi=0.7)

ocv_cov_by_dose = function(vc1=0.95, vc2=0.95, vacc_overlap=0.7){
  # pi represents the proportion of the first-dose recipients who went on to
  # be vaccinated for the second dose
  pi = vacc_overlap
  if (vacc_overlap < (vc1+vc2-1)/vc1) {
    message("vacc_overlap must be larger than (vc1+vc2-1)/vc1, ",
            (vc1+vc2-1)/vc1)
    vacc_overlap = (vc1+vc2-1)/vc1 + 0.02
    message("vacc_overlap is set at ", vacc_overlap)
  }
  else if (vacc_overlap > (vc2/vc1)) {
    message("vacc_overlap must be smaller than ", vc2/vc1)
    vacc_overlap = vc2/vc1 - 0.02
    message("vacc_overlap is set at ", vacc_overlap)
  }
  two = vc1*vacc_overlap
  # vr1plus = vc1*pi + vc1*(1-pi) + vc2*(1-pi*vc1/vc2)
  oneplus = two + vc1*(1-vacc_overlap) + vc2*(1-vacc_overlap*vc1/vc2)
  one = oneplus - two

  return (list(oneplus=oneplus, two=two, one=one))
}

