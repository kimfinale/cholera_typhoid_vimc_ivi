#' Calculates the case fatality ratio using IHME GBD results
#'
#' The \code{calc_incid_rate_risk_adj_chol()} overall incidence rate over the
#' years adjusted by the proportion of population who is categorized by
#' WASH variable

#' @param country Target country
#' @param ref_year Reference year
#' @param wash_risk_ratio Risk ratio (odds ratio) for cholera by WASH variable
#' @param wash_prop Proportion of population with access to improved WASH
#' variables
#' @export
#' @examples
#'
calc_incid_rate_risk_adj_chol <- function(country,
                                     overall_ir,
                                     wash_risk_ratio,
                                     wash_prop,
                                     ref_year = 2010) {

  cn <- country
  rm(country)

  # incidence rate adjusted by WASH risk (row) over 2000-2100 (101 column)
  incid_rate_adj <-
    data.frame(matrix(NA, nrow = 101, ncol = 101))
  names(incid_rate_adj) <- c(as.character(2000:2100))

  wash_prop %>% filter(country == cn) -> wash_prop_country
  # wash_prop_country <- wash_prop[wash_prop$country == cn,]

  # risk_vars <- wash_risk_ratio$var
  # remove columns with NAs
  # first two risk variables (at least basic ~) were removed
  # Should I create global variable risk_vars_cholera and risk_vars_typhoid?
  risk_vars <-
    names(wash_prop_country[,5:10])[!is.na(colSums(wash_prop_country[,5:10]))]

  ir_adj <-
    data.frame(matrix(NA, nrow = length(risk_vars), ncol = 101 + 1))
  names(ir_adj) <- c("WASH_var", as.character(2000:2100))
  ir_adj$WASH_var <- risk_vars

  for (rv in risk_vars) {
    ir_by_risk <- calc_incid_rate_by_risk(country = cn,
                                          overall_ir = overall_ir,
                                          wash_risk_ratio = wash_risk_ratio,
                                          wash_prop = wash_prop,
                                          ref_year = ref_year,
                                          risk_var = rv)
    # cat("country =", cn, ", risk variable =", rv, "\n")
    # extract predicted proportion of low-risk people over 2000-2100
    wash_prop_country %>% pull(rv) -> prop_low_risk

    ir_adj[ir_adj$WASH_var == rv, 2:102] <-
      ir_by_risk$ir_high * (1 - prop_low_risk) +
      ir_by_risk$ir_low * prop_low_risk
    # cat("ir =", ir_ref$ir_high, ir_ref$ir_low, "\n")
  }
  incid_rate_adj[1, ] <- apply(ir_adj[, 2:102], 2, mean, na.rm = TRUE)
  # incidence rates remain same across age groups
  for (r in 2:101) {
    incid_rate_adj[r, ] <- incid_rate_adj[1, ]
  }
  return (incid_rate_adj)
}

