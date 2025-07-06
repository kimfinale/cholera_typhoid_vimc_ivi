#' Calculate incidence rates for those with access to protective factor (e.g.,
#' at least basic sanitation) and those without for a given overall incidence
#' rate and the proportion of population for each category
#'
#' The \code{calculate_YLD()} computes YLD.
#' @param life_expectancy_data UN life expectancy data (0, 1, 5, 10 yo, every 5 yrs)
#' @param country Character vector of country names to simulate
#' @param year Numeric vector of years for which simulation
#' @export

get_life_expectancy <- function(life_expectancy_data = NULL,
                                country = NULL,
                                year = NULL) {
  if (is.null(life_expectancy_data)) {
    # life_expectancy_data <-
    #   fread("inst/extdata/202110gavi-1_dds-201910_2_life_ex_both.csv")
    life_expectancy_data <-
      fread("inst/extdata/202310gavi-4_dds-202208_life_ex_both.csv")
  }
  cntry <- clean_country_names(country)
  rm(country)
  yr <- year
  rm(year)

  # life_expectancy_data %>%
  #   filter(country == cntry, year %in% yr) %>%
  #   select(age_from, year, value) %>%
  #   tidyr::pivot_wider(id_cols = c(age_from, year), names_from = year) -> d1
#   The above code produced the following error and rewritten as below.
#   Error in `tidyr::pivot_wider()`:
#     ! `id_cols` can't select
#   a column already selected by
#   `names_from`.
# ℹ Column `year` has already been
#   selected.

  life_expectancy_data %>%
    dplyr::filter(country == cntry, year %in% yr) %>%
    dplyr::select(age_from, year, value) %>%
    tidyr::pivot_wider(id_cols=c(age_from), names_from=year, values_from=value) -> d1


  ## add rows as the age categories are different
  d2 <- d1[c(1, rep(2, 4), rep(3:(nrow(d1)-1), each = 5), nrow(d1)), ]
  d2 <- d2[, -1] # remove age_from column
  # add columns as year categories are different
  life_expectancy <- d2[, c(rep(seq_len(ncol(d2)), each = 5), ncol(d2))]
  names(life_expectancy) <- as.character(yr)

  return(life_expectancy)
}
