% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{prop_basic_san}
\alias{prop_basic_san}
\title{Proportion of population who have access to basic sanitation services
Estimates based on 2000-2017 data from WHO/UNICEF JMP.
Estimatrers were fit to a saturating exponential function: f(x; k, tau) = 1 - exp (-k(x-tau))}
\format{
'data.frame':	405 obs. of  3 variables:
\describe{
  \item{country }{country name}
  \item{year}{starting age}
  \item{pred}{estimates for the proportion of population who have access to basic sanitation services}
  ...
}
}
\source{
\url{https://washdata.org/data/downloads#WLD}
}
\usage{
prop_basic_san
}
\description{
Proportion of population who have access to basic sanitation services
Estimates based on 2000-2017 data from WHO/UNICEF JMP.
Estimatrers were fit to a saturating exponential function: f(x; k, tau) = 1 - exp (-k(x-tau))
}
\keyword{datasets}
