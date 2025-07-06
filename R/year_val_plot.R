#' Ploting year vs. variable values
#'
#' The \code{year_val_plot()} filiters and plots the value of interest
#' @param x VIMC long-format result
#' @param disease disease (i.e., either Typhoid or Cholera)
#' @param country country of interest in long name format
#' @param age age set to some value if you want to see the results for a particular age. If missing, results are summed across ages
#' @export
#' @import tidyr, dplyr, ggplot2
#' @examples
#' year_val_plot()
#'
# Very simple diagnostic plots -- maybe helpful to detect in case there are major flaws
year_val_plot <- function(disease=NULL,
                          country = NULL,
                          vacc_scenario = NULL,
                          sim_central_value,
                          age){
  country_clean <- clean_country_names(country)
  if (is.null(disease)) {
    stop("Disease name must be provided")
  }
  dis <- disease
  rm(disease)

  if (missing(age)) {
    sim_central_value %>%
      filter(tolower(disease) == tolower(dis), country_name == country_clean) %>%
      group_by(year) %>%
      summarize(cohort_size = sum(cohort_size),
                cases = sum(cases),
                deaths = sum(deaths),
                dalys = sum(dalys)) %>%
      pivot_longer(cols = cohort_size:dalys, names_to = "var", values_to = "val") -> df

    age <- NA

  } else {
    sim_central_value %>%
      filter(tolower(disease) == tolower(dis),
             age == !!age, country_name == country_clean) %>%
      pivot_longer(cols = cohort_size:dalys, names_to = "var", values_to = "val") -> df

  }

  plt <- ggplot(df, aes(year, val))+
      geom_line()+
      facet_wrap(~var, scales = "free_y")
  if (is.na(age)) {
    plt <- plt + ggtitle(paste0(dis, " in ", country_clean, " (", vacc_scenario, ")"))
  }
  else {
    plt <- plt + ggtitle(paste0(dis, " in ", country_clean, " (", vacc_scenario, ")", " (age = ", age, ")"))
  }

  return (plt)
}
