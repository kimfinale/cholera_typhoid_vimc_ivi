#' VIMC report for individual runs from stochastic simulation
#'
#' The \code{vimc_stoch_report()} reshapes simulation results (e.g., data frame in the wide format or a list of wide-format data frames into the long format
#' @param template The VIMC template used
#' @param sim_output The population data of countries which includes the file
#' @export
#' @import tidyr
#' @examples
#' vimc_stoch_report()
#'
vimc_stoch_report <- function (
  disease = NULL,
  country = NULL,
  sim_output,
  year = 2000:2100,
  age = 0:100,
  value_name,
  column_order = c("disease", "run_id", "year", "age", "country", "country_name", "cohort_size", "cases", "deaths", "dalys")) {

  nvar <- length(sim_output$var) ## number of variables (cohort_size, cases, deaths, dalys)
  runs <- length(sim_output$val[[2]]) ## first output is cohort size and same across the runs
  list2 <- list()
  for(i in 1:nvar){
    list1 <- list()
    for(j in seq_len(runs)){
      if(i == 1){
        df <- sim_output$val[[i]]
      } else {
        df <- sim_output$val[[i]][[j]]
      }
      df$age <- age
      df$country_name <- clean_country_names(country)
      df$disease <- disease
      df$country <- get_iso3(country)
      df$run_id <- j
      df_long <- tidyr::pivot_longer(df, cols = as.character(year), names_to = "year", values_to = sim_output$var[i])
      df_long$year <- as.integer(df_long$year) # from character to integer
      df_long <- dplyr::relocate(df_long, any_of(column_order))
      list1[[j]] <- df_long
    }
    list2[[i]] <- do.call("rbind", list1)
  }

  df2 <- list2[[1]]
  for(k in 2:nvar){
    df2 <- left_join(df2, list2[[k]], by = c("disease", "run_id", "year", "age", "country", "country_name"))
  }

  return(df2)
}

