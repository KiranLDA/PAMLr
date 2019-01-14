#' Calculate altitude from pressure
#'
#' @description This opens a java application which allows the user to zoom in and out. In Rstudio it will open in the viewer pane and in base R in an html readers. Note that this can be a bit slow
#'
#' @param P on-bird pressure in hectopascals
#' @param P0 pressure at sea level in hectopascals
#' @param T0 temperature at sea level in kelvin (i.e. [celcius + 273.15] OR [(Farenheit − 32) * 5/9 + 273.15])
#' @return altitude in metres, default is calculated according to International Standard Atmosphere model (International Organization for Standardization 1975: ISO 2533:1975)
#'
#' @examples
#' #load dummy data
#' data(hoopoe)
#' PAM_data=hoopoe
#'
#' altitude = altitudeCALC(P = PAM_data$pressure$obs)
#' plot(PAM_data$pressure$date, altitude, type="o",pch=16,
#'      xlab="Date", ylab="Altitude (m)")
#'
#' @export
altitudeCALC <- function( P,
                          T0 = 288.15,
                          P0 = 1013.25) {
  L = -0.0065
  altitude = -(T0 / L)*(1-((P/P0)^(1/5.2561)))
  return(altitude)
}
