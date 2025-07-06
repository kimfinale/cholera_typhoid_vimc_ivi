#' Run models to compute cohort size, cases, deaths, dalys
#' The \code{vaccine_impact()} runs les
#' @param data IHME data age category <1 year, 1 to 4, 5 to 9, ..., 90 to 94
#' @param variable central named "val" and "upper", "lower" bounds
#' @export
#' @examples

# make age categories as in the incidence rate data (ir_data)
spread_age_IHME <- function(data, age_total = 101, variable = "val") {
  var <- match.arg(variable, c("val", "upper", "lower"))
  cat_first <- "<1 year"
  # next category starts "1 to 4", "5 to 9", ..., "90 to 94"
  age0 <- c(1, seq(5, 90, by = 5))
  age1 <- c(4, seq(9, 94, by = 5))
  age_cat <- c(paste0(age0, " to ", age1))

  new_data <- rep(NA, age_total)
  new_data[1] <- unlist(data[data$age == cat_first, var])
  for (ag in age0) {
    new_data[(ag+1):(ag+5)] <-
      unlist(data[data$age == age_cat[which(age0 == ag)], var])
  }
  # 95-100 age groups were set to be the same as "90 to 94"
  new_data[96:age_total] <- new_data[95]

  return(new_data)
}
