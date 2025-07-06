#' VIMC report
#'
#' The \code{vimc_report()} takes the mean across stochastic runs and reshapes simulation results (e.g., data frame in the wide format or a list of wide-format data frames into the long format
#' @param template The VIMC template used
#' @param sim_output The population data of countries which includes the file
#' @export
#' @import tidyr
#' @examples
#' vimc_report()
#'
vimc_report <- function (disease = NULL,
                         country = NULL,
                         sim_output,
                         year = 2000:2100,
                         age = 0:100,
                         value_name) {
  ## means of elements across the
  if((!is.data.frame(sim_output)) & is.list(sim_output)){
    arr <- array(unlist(sim_output), dim = c(dim(sim_output[[1]]), length(sim_output)))
    df <- as.data.frame(rowMeans(arr, na.rm = TRUE, dims = 2))
  } else {
    df <- sim_output
  }
  names(df) <- as.character(year)
  df$age <- age
  df$country_name <- clean_country_names(country = country)
  df$disease <- disease
  df$country <- get_iso3(country = country)

  df_long <- tidyr::pivot_longer(df, cols = as.character(year), names_to = "year")
  df_long$year <- as.integer(df_long$year) # from character to integer
  names(df_long) <- c(names(df_long)[1:5], value_name)
  ## adjust the order of columns according to the template
  report_column_order <- c("disease", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")
  dplyr::relocate(df_long, any_of(report_column_order))

  return(df_long)
}

