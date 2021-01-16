#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{clean_country_names()} is used to clean and standardize country names according to VIMC report templates
#' population for both sexes and incidence rate
#' @param country A vector of country names in character
#' @export
#' @examples
#' country <- clean_country_names(country = "DRC")
#' # Congo, the Democratic Republic of the
clean_country_names <- function(country){
  for (i in 1:length(country)) {
    if (country[i] %in% c("DR Congo", "Democratic Republic of the Congo", "DRC", "Congo, Dem. Rep.", "Congo, DR",  "Congo, the Democratic Republic of the")){
      country[i] <- "Congo, Democratic Republic of the"
    }
    if (country[i] %in% c("Congo, Rep.", "Republic of the Congo")){
      country[i] <- "Congo, Republic of the"
    }
    if (country[i] %in% c("São Tomé and Príncipe")){
      country[i] <- "Sao Tome e Principe"
    }
    if (country[i] %in% c("Iran", "Iran, Islamic Rep.", "Iran (Islamic Republic of)")){
      country[i] <- "Iran, Islamic Republic of"
    }
    if (country[i] %in% c("North Korea", "Korea:North", "Korea, DPR", "DPRK", "Democratic People's Republic of Korea")){
      country[i] <- "Korea, Democratic People's Republic of"
    }
    if (country[i] %in% c("South Korea", "Korea:South", "Korea, Rep.")){
      country[i] <- "Korea, the Republic of"
    }
    if (country[i] %in% c("Sudan: South")){
      country[i] <- "South Sudan"
    }
    if (country[i] %in% c("Sudan: North")){
      country[i] <- "Sudan"
    }
    if (country[i] %in% c("Venezuela", "Venezuela, RB", "Venezuela (Bolivarian Republic of)")){
      country[i] <- "Venezuela, Bolivarian Republic of"
    }
    if (country[i] %in% c("Tanzania", "United Republic of Tanzania")){
      country[i] <- "Tanzania, United Republic of"
    }
    if (country[i] %in% c("Syria")){
      country[i] <- "Syrian Arab Republic"
    }
    if (country[i] %in% c("Moldova")){
      country[i] <- "Moldova, Republic of"
    }
    if (country[i] %in% c("CAR")){
      country[i] <- "Central African Republic"
    }
    if (country[i] %in% c("Lao", "Laos", "Lao PDR")){
      country[i] <- "Lao People's Democratic Republic"
    }
    if (country[i] %in% c("US", "USA")){
      country[i] <- "United States of America"
    }
    if (country[i] %in% c("C?te d'Ivoire", "CÃ´te d'Ivoire")){
      country[i] <- "Cote d'Ivoire"
    }
  }
  return (country)
}
