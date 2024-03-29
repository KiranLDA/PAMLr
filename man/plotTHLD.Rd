% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTHLD.R
\name{plotTHLD}
\alias{plotTHLD}
\title{Plot threshold}
\usage{
plotTHLD(dta, classification, threshold, type, new_window = FALSE, ...)
}
\arguments{
\item{dta}{Raw acceleration or pressure data used to make the classification}

\item{classification}{Is the result of the classification. is in the format of a vector of numbers used for low or high activity}

\item{threshold}{The threshold between different classes}

\item{type}{The type of classification used i.e "flapping" or soar-gliding}

\item{new_window}{whether you want to plot it in a new window or not}

\item{...}{any additional parameters used by graphics::hist}
}
\value{
a graphic with the output from the classifications
}
\description{
Plot threshold
}
\examples{
##specify the data location
#data(hoopoe)
#PAM_data=hoopoe

#PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#& (PAM_data$acceleration$date <= "2017-06-01")),]

#behaviour = classifyFLAP(dta = PAM_data$acceleration,
#                         period = 3,
#                         toPLOT = FALSE)

#plotTHLD(dta = PAM_data$acceleration$act,
#         type = "flapping",
#         classification = behaviour$classification,
#         threshold = behaviour$threshold)

}
