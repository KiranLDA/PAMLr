#' Adjust time for clock drift
#'
#' Linearly rescale a sequence of dates to a new start and end time
#' to correct for clock drift in the tag.
#'
#' @title Clock Drift Adjustment
#' @param time a vector of POSIXct times.
#' @param start new start time as POSIXct.
#' @param end new end time as POSIXct.
#'
#' @examples
#' #dates = hoopoe$magnetic$date
#' #drift = 12
#' #adjusted_end = as.POSIXct(dplyr::last(hoopoe$magnetic$date) + 12*60, tz="UTC")
#' #drift_corrected = clockDRIFT(time = dates,
#' #                             start= dates[1],
#' #                             end = adjusted_end)
#'
#' @importFrom stats approx
#'
#' @export
clockDRIFT <- function(time,
                       start,
                       end) {
  print("Error: This function is deprecated, use calculate_clockdrift, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")
  # ntimes <- length(time)
  # corrected_time <- .POSIXct(approx(time[c(1,ntimes)],c(start,end),time)$y)
  # return(corrected_time)
}
