#' Calculate altitude from pressure
#'
#' @description  This function calculated altitude from atmospheric pressure
#'
#' @param P on-bird pressure in hectopascals
#' @param P0 pressure at sea level in hectopascals
#' @param T0 temperature at sea level in kelvin (i.e. celcius + 273.15 OR (Farenheit − 32) * 5/9 + 273.15)
#'
#' @return altitude in metres, default is calculated according to International Standard Atmosphere model (International Organization for Standardization 1975: ISO 2533:1975)
#' @references Atmosphere, S., 1975. International organization for standardization. ISO, 2533, p.1975.
#'
#' @export
altitudeCALC <- function( P,
                          T0 = 288.15,
                          P0 = 1013.25) {
  print("Error: This function is deprecated, use calculate_altitude, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")

  # L = -0.0065
  # altitude = -(T0 / L)*(1-((P/P0)^(1/5.2561)))
  # return(altitude)
}
