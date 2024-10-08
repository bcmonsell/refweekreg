% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar_mean_adj.r
\name{calendar_mean_adj}
\alias{calendar_mean_adj}
\title{Calendar mean adjust regressor}
\usage{
calendar_mean_adj(this_reg = NULL)
}
\arguments{
\item{this_reg}{double precision time series array; a regressor from which the calendar month mean will be removed.
This is a required entry.}
}
\value{
double precision time series array of the regressor with the calendar month mean removed
}
\description{
Remove the calendar month mean of a given regressor expressed as a time series object.
}
\details{
Version 2.0, 5/6/2024
}
\examples{
gf_years <- c(2001, 2006, 2017, 2022)
this_gf <-
     gen_rw_regressors(gf_years, 4, 2000, 2027, 'gf', remove_cal_means = FALSE)
this_gf_mean_adj <- calendar_mean_adj(this_gf)
}
\author{
Brian C. Monsell, \email{monsell.brian@bls.gov} or \email{bcmonsell@gmail.com}
}
