#' Calculates the case fatality ratio using IHME GBD results
#'
#' The \code{calc_cfr()} calcuates case fatality ratio using the IHME GBD results
#' @param country Target country
#' @param incid Incidence rate
#' @param ref_year Reference year
#' @param var_nae Variable name to extract val (mean), upper, or lower
#' @export
#' @examples
#' calc_cfr <- function(country = "Afghanistan", ref_year = 2017)
#'
#' this needs to be modified to account for the uncertainty
calc_age_spec_ir_ref_tf <- function(country,
                                    incid,
                                    ref_year = 2017,
                                    var_name = "val") {
  var <- match.arg(var_name, c("val", "upper", "lower"))
  cntry <- country
  rm(country)

  incid %>%
    filter(metric_name == "Rate", location_name == cntry, year == ref_year) -> dat

  overall_ir <- dat %>% filter(age_name == "All Ages") %>% pull(var)

  cat_first <- "<1 year"
  cat_last <- "95 plus"
  age0 <- c(1, seq(5, 90, by = 5))
  age1 <- c(4, seq(9, 94, by = 5))
  age_cat <- c(paste0(age0, " to ", age1))

  ir_age <- rep(NA, 101) # 0-100 yo by 1 year
  ir_age[1] <- dat %>% filter(age_name == cat_first) %>% pull(val)

  for (i in seq_along(age0)) {
    dat %>% filter(age_name == age_cat[i]) %>% pull(var) -> ir
    ir_age[(age0[i]+1):(age1[i]+1)] <- ir
  }
  dat %>% filter(age_name == cat_last) %>% pull(var) -> ir
  ir_age[96:101] <- ir

  ir_dist = ir_age / overall_ir

  return (list(ir_dist = ir_dist, overall_ir = overall_ir))
}

