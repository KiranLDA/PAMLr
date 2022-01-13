#' Calculate water pressure from depth
#'
#' @description  This function calculates water pressure from depth
#'
#' @param h depth in m *must be positive
#' @param p water density for seawater is 1023.6kg/m3 and freshwater 997.0474 kg/m3
#' @param g  gravitational field strength default 9.80665 m/s2
#'
#' @return pressure in hectopascals
#'
#' @examples
#' pressure = calculate_water_pressure(113.16)
#'
#' @export
calculate_water_pressure <- function(h, #depth in m
                                     p = 1023.6, # water density for seawater is 1023.6kg/m3 and freshwater 997.0474 kg/m3
                                     g = 9.80665 #m/s2
) {
  P = (p*g*h)/100 # to convert to hPa
  return(P)
}
