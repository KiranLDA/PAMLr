% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_roll.R
\name{calculate_roll}
\alias{calculate_roll}
\alias{calculate_pitch}
\alias{calculate_yaw}
\title{triaxial calculations}
\usage{
calculate_roll(dta)

calculate_pitch(dta)

calculate_yaw(dta)
}
\arguments{
\item{dta}{magentic data from PAM logger see hoopoe$magnetic for an example}
}
\value{
roll, pitch and yaw from acceleration data
}
\description{
Utilities for calculating roll, pitch and yaw
}
\details{
Calculate roll, pitch and yaw
}
\examples{
calculate_roll(dta = swift$magnetic)
calculate_pitch(dta = swift$magnetic)
calculate_yaw(dta = swift$magnetic)

}
\references{
Bidder, O.R., Walker, J.S., Jones, M.W., Holton, M.D., Urge, P., Scantlebury, D.M., Marks, N.J., Magowan, E.A., Maguire, I.E. and Wilson, R.P., 2015. Step by step: reconstruction of terrestrial animal movement paths by dead-reckoning. Movement ecology, 3(1), p.23.
}
