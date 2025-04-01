#' Utilities for calculating roll, pitch and yaw
#'
#' Calculate roll, pitch and yaw
#' @title triaxial calculations
#' @rdname calcls
#' @param dta magentic data from PAM logger see hoopoe$magnetic for an example
#' @return roll, pitch and yaw from acceleration data
#' @references Bidder, O.R., Walker, J.S., Jones, M.W., Holton, M.D., Urge, P., Scantlebury, D.M., Marks, N.J., Magowan, E.A., Maguire, I.E. and Wilson, R.P., 2015. Step by step: reconstruction of terrestrial animal movement paths by dead-reckoning. Movement ecology, 3(1), p.23.
#'
#' @examples
#' calculate_roll(dta = swift$magnetic)
#' calculate_pitch(dta = swift$magnetic)
#' calculate_yaw(dta = swift$magnetic)
#'
#' @export
calculate_roll <- function(dta){
  Sx = dta$gX#mX#/10000
  Sy = dta$gY#mY#/10000
  Sz = dta$gZ#mZ#/10000
  roll  = atan2(Sx,(sqrt(Sy^2 + Sz^2)))*(180/pi)
  return(roll)
}
#' @rdname calcls
#' @export
calculate_pitch <- function(dta){
  Sx = dta$gX#mX#/10000
  Sy = dta$gY#mY#/10000
  Sz = dta$gZ#mZ#/10000
  pitch = atan2(Sy,(sqrt(Sx^2 + Sz^2)))*(180/pi)
  return(pitch)
}
#' @rdname calcls
#' @export
calculate_yaw <- function(dta){
  Sx = dta$gX#mX#/10000
  Sy = dta$gY#mY#/10000
  Sz = dta$gZ#mZ#/10000
  yaw   = atan2(Sz,(sqrt(Sx^2 + Sy^2)))*(180/pi)
  return(yaw)
}
