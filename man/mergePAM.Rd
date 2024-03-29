% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergePAM.R
\name{mergePAM}
\alias{mergePAM}
\title{Merges all sensors into one}
\usage{
mergePAM(dta, all = TRUE, interp = FALSE)
}
\arguments{
\item{dta}{raw pam data see \verb{data(bee_eater} for example}

\item{all}{logical. Default TRUE. Whether or not to keep NAs (i.e. all the datasets)}

\item{interp}{logical. Default FALSE. whether or not to interpolate if there are NAs. if all = FALSE then interp is not used.}
}
\value{
merged and interpolated dataset
}
\description{
This function takesthe typical PAM_data input, which is a nested list of different sensor data, all formatted at different time resolutions, and merges them all into one big table. By default all times are kept, and not interpolated.
}
\examples{
#data(bee_eater)
#PAM_data = bee_eater
#merged_dta = mergePAM(PAM_data, interp = TRUE)
#head(merged_dta)

}
