#' Calculate incidence rates for those with access to protective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param life_exp_data Life expectancy data from UN for age 0, 1, 5, 10, 15, ..
#' year 2000, 2005, 2010, ...
#' @param country Name of the country
#' @param year Years for which life expectancy is calculated
#' @export
#' @examples
#' life_exp_data <- fread("inst/extdata/201910gavi-5_dds-201910_2_life_ex_both.csv")
#' get_life_exp(life_exp_data = life_exp_data, country = "Burkina Faso", year = 2000:2100)
#'

get_life_exp <- function(life_exp_data, country, year){
  cntry <- country
  rm(country)
  yr <- year
  rm(year)
  life_exp_data %>%
    dplyr::filter(country == cntry, year %in% yr) %>%
    dplyr::select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year) -> life_exp1
  ## life expectancy is given by 5-yr age group and every 5 yrs
  ## make it by 1-yr age group every year by repeating the same value for the
  ## intervening years
  life_exp2 <- life_exp1[c(1, rep(2,4), rep(3:21, each = 5), 22), ] ## add rows
  life_exp2 <- life_exp2[, -1] # remove age column
  life_exp <- life_exp2[, c(rep(seq_len(ncol(life_exp2)-1), each = 5), rep(ncol(life_exp2), 6))] # add columns
  names(life_exp) <- as.character(yr)
  return(life_exp)
}
