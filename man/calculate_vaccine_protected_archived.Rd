% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_vaccine_protected_archived.R
\name{calculate_vaccine_protected_archived}
\alias{calculate_vaccine_protected_archived}
\title{Vaccinate population for a set of condition}
\usage{
calculate_vaccine_protected_archived(
  country = "Pakistan",
  population,
  year = 2000:2100,
  vacc_efficacy = 0.82,
  vacc_duration_effect = 6,
  vacc_coverage = 0.85,
  vacc_age = 1:15,
  vacc_year = seq(2020, 2030, 3)
)
}
\arguments{
\item{year}{Year in which vaccination is implemented}

\item{efficacy}{Vaccine efficacy as a proportion}

\item{duration_effect}{Years during which vaccine-induced immunity remains}

\item{coverage}{A proportion of target population that receives the vaccine}

\item{age_range}{Target age range}
}
\description{
Vaccinate population for a set of condition
}
\examples{
pop <- setup_population(); compute_cases(pop)
}
