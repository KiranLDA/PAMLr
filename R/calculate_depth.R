#' Calculate depth from pressure
#'
#' @description  This function calculates depth from pressure
#'
#' @param P pressure in hectopascals
#' @param p water density for seawater is 1023.6kg/m3 and freshwater 997.0474 kg/m3
#' @param g gravitational field strength default 9.80665 m/s2
#'
#' @return depth in m
#'
#' @examples
#' depth = calculate_depth(11065)
#'
#' @export
calculate_depth <- function(P, #in hPa
                            p = 1023.6, # water density for seawater is 1023.6kg/m3
                            g = 9.80665){
  P=P*100 #convert to N/m3
  h = P/(p*g)
  return(h)
}
