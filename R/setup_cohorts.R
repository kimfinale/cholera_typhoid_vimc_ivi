#' Base population setup
#'
#' The \code{setup_population ()} sets up cohorts using the interpolated population for both sexes
#' age distribution in 2020
#' @param country The of country of interest for which you want to set up the population in
#' @param population_data The population data of countries which includes the file
#' @param year A range of years during which poul
#' @export
#' @import data.table
#' @examples
#' setup_population()
#'
setup_cohorts <- function(country = NULL,
                         year = 2000:2100,
                         rel_risk_low = 0.0,
                         population_data ="data/201910gavi-5_dds-201910_2_int_pop_both.csv",
                         prop_san_data = "data/JMP2019.csv"){



  if(is.null(country)){
    stop("Country name must be provided")
  }
  if(!exists("pop")){
    pop <- data.table::fread(population_data)
  }
  if(!exists("prop")){
    prop <- data.table::fread(prop_san_data)
  }

  pop <- gavi201910_int_pop_both
  prop <- prop_basic_san
  cntry <- country
  rm(country)
  # cols <- as.character(year)
  # pop <- pop[country == country, ..cols] # data in the wide format
  pop_wide <- pop %>% # data in the long format
    dplyr::filter(country == cntry, year %in% !!year) %>%
    dplyr::select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year, values_from = value)
  pop_wide <- pop_wide[,-1]
  names(pop_wide) <- as.character(names(pop_wide))
  prop_san <- prop %>% dplyr::filter(country == cntry) # proportion of population with at least basic sanitation

  if(sum(year %in% prop_san$year) > length(prop_san$year)){
    warning("High risk proportion inputs do not cover the period of population projections")
  }
  pop_low_risk <- as.data.frame(pop_wide)
  pop_high_risk <- pop_low_risk
  for (i in 1:ncol(pop_wide)) {
    pop_high_risk[, i] <- pop_high_risk[, i] * (1 - prop_san$prop[i])
    pop_low_risk[, i] <- pop_low_risk[, i] * prop_san$prop[i]
  }
  pop_risk_adj <- pop_high_risk + rel_risk_low * pop_low_risk
  pop_risk_adj <- pop_risk_adj %>% dplyr::mutate(across(everything(), as.integer))

  return (pop_risk_adj)
}
