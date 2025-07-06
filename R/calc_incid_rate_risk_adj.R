#' Calculates the case fatality ratio using IHME GBD results
#'
#' The \code{calc_cfr()} calcuates case fatality ratio using the IHME GBD results
#' @param country Target country
#' @param ref_year Reference year
#' @export
#' @examples
#' calc_cfr <- function(country = "Afghanistan", ref_year = 2017)
#'
#' this needs to be modified to account for the uncertainty
calc_incid_rate_risk_adj <- function(country,
                                     overall_ir,
                                     wash_risk_ratio,
                                     wash_prop,
                                     ref_year = 2017,
                                     incid = incid_tf_ihme) {

  cntry <- country
  rm(country)
  incid_data <- incid
  rm(incid)

  age_spec_ir_ref <- calc_age_spec_ir_ref_tf(country = cntry,
                                             incid = incid_data,
                                             ref_year = 2017)

  age_spec_ir <- age_spec_ir_ref$ir_dist * overall_ir

  # incidence rate adjusted by WASH risk (row) over 2000-2100 (101 column)
  incid_rate_adj <-
    data.frame(matrix(NA, nrow = length(age_spec_ir), ncol = 101))
  names(incid_rate_adj) <- c(as.character(2000:2100))

  wash_prop %>% filter(country == cntry) -> wash_prop_country

  # risk_vars <- wash_risk_ratio$var
  # remove columns with NAs
  risk_vars <-
    names(wash_prop_country[,3:10])[!is.na(colSums(wash_prop_country[,3:10]))]

  for (i in 1:length(age_spec_ir)) {
    ir_adj <-
      data.frame(matrix(NA, nrow = length(risk_vars), ncol = 101 + 1))
    names(ir_adj) <- c("WASH_var", as.character(2000:2100))
    ir_adj$WASH_var <- risk_vars

    for (rv in risk_vars) {
      ir_by_risk <- calc_incid_rate_by_risk(country = cntry,
                                            overall_ir = age_spec_ir[i],
                                            wash_risk_ratio = wash_risk_ratio,
                                            wash_prop = wash_prop,
                                            ref_year = 2017,
                                            risk_var = rv)
      # cat("country =", cntry, ", risk variable =", rv, "\n")
      # extract predicted proportion of low-risk people over 2000-2100
      wash_prop_country %>% pull(rv) -> prop_low_risk

      ir_adj[ir_adj$WASH_var == rv, 2:102] <-
        ir_by_risk$ir_high * (1 - prop_low_risk) +
        ir_by_risk$ir_low * prop_low_risk
      # cat("ir =", ir_ref$ir_high, ir_ref$ir_low, "\n")
    }
    incid_rate_adj[i, ] <- apply(ir_adj[, 2:102], 2, mean, na.rm = TRUE)
  }
  return (incid_rate_adj)
}

