#' quickly plot the pam data to have an idea of it's quality
#'
#' @param dta path where files are stored
#' @param measurements a series of measurements logged by the PAM logger which are to be plotted. Currently supports these file extentions: "pressure","light", "acceleration", "temperature" and "magnetic"
#' @param ... any additional parameters used by graphics::plot
#'
#' @return a plot of PAM data
#'
#' @examples
#' data(PAM_data)
#' str(PAM_data)
#'
#' #plot everything in 2 windows
#' quickPLOT(PAM_data)
#'
#' # only subset some measurements
#' quickPLOT(PAM_data, measurements = c("light", "pressure", "acceleration"))
#'
#' @importFrom graphics plot par
#' @export
quickPLOT <- function(dta,
                      measurements = c("pressure","light", "acceleration", "temperature", "magnetic"), ...){
  # for testing the function
  # dta = PAM_data
  # measurements = c("pressure","light", "acceleration", "temperature", "magnetic")

  rows = 0
  if("light" %in% measurements) rows = rows + 1
  if("pressure" %in% measurements) rows = rows + 1
  if("temperature" %in% measurements) rows = rows + 1
  if("acceleration" %in% measurements) rows = rows + 2

  par(mfrow = c(rows,1))
  if("light" %in% measurements){
    plot(dta$light$date, dta$light$obs, type="l", xlab= "Date", ylab = "Light",...)
  }
  if("pressure" %in% measurements){
    plot(dta$pressure$date, dta$pressure$obs, type="l", xlab= "Date", ylab = "Pressure (hPa)",...)
  }
  if("temperature" %in% measurements){
    plot(dta$temperature$date, dta$temperature$obs, type="l", xlab= "Date", ylab = "Temperature (c)",...)
  }
  if("acceleration" %in% measurements){
    plot(dta$acceleration$date, dta$acceleration$act, type="l", xlab= "Date", ylab = "Activity (z-axis jiggle)",...)
    plot(dta$acceleration$date, dta$acceleration$pit, type="l", xlab= "Date", ylab = "Pitch (z-axis angle)",...)
  }
  if("magnetic" %in% measurements){
    par(mfrow = c(6,1))

    plot(dta$magnetic$date, dta$magnetic$gX, type="l", xlab= "Date", ylab = "gX",...)
    plot(dta$magnetic$date, dta$magnetic$gY, type="l", xlab= "Date", ylab = "gY",...)
    plot(dta$magnetic$date, dta$magnetic$gZ, type="l", xlab= "Date", ylab = "gZ",...)

    plot(dta$magnetic$date, dta$magnetic$mX, type="l", xlab= "Date", ylab = "mX",...)
    plot(dta$magnetic$date, dta$magnetic$mY, type="l", xlab= "Date", ylab = "mY",...)
    plot(dta$magnetic$date, dta$magnetic$mZ, type="l", xlab= "Date", ylab = "mZ",...)
  }
}
