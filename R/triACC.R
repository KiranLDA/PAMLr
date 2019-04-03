#' Calculate roll, pitch and yaw from tri axial acceleration data
#'
#' @param dta magentic data from PAM logger see hoopoe$magnetic for an example
#'
#' @return roll, pitch and yaw from acceleration data
#'
#' @examples
#' data(swift)
#' PAM_data = swift
#'
#' triACC(dta = PAM_data$magnetic)
#'
#' @export
triACC <- function(dta){

  # acceleration conversion
  Sx = dta$mX#/10000
  Sy = dta$mY#/10000
  Sz = dta$mZ#/10000

  roll  = atan2(Sx,(sqrt(Sy^2 + Sz^2)))*(180/pi)
  pitch = atan2(Sy,(sqrt(Sx^2 + Sz^2)))*(180/pi)
  yaw   = atan2(Sz,(sqrt(Sx^2 + Sy^2)))*(180/pi)

  return(list(roll=roll,
              pitch = pitch,
              yaw = yaw))
}
