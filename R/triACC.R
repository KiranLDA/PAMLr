#' Tri-axial acceleration functions
#'
#' @description This function calculates roll, pitch and yaw from tri-axial acceleration data. It also centeres the acceleration data (for plotting on a sphere).
#'
#' @param dta magentic data from PAM logger see hoopoe$magnetic for an example
#'
#' @return roll, pitch and yaw from acceleration data
#'
#' @references Bidder, O.R., Walker, J.S., Jones, M.W., Holton, M.D., Urge, P., Scantlebury, D.M., Marks, N.J., Magowan, E.A., Maguire, I.E. and Wilson, R.P., 2015. Step by step: reconstruction of terrestrial animal movement paths by dead-reckoning. Movement ecology, 3(1), p.23.
#'
#' @examples
#' #data(swift)
#' #PAM_data = swift
#'
#' #triACC(dta = PAM_data$magnetic)
#'
#' @export
triACC <- function(dta){
  print("Error: This function is deprecated, use calculate_triaxial_acceleration, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")

  # # acceleration conversion
  # Sx = dta$mX#/10000
  # Sy = dta$mY#/10000
  # Sz = dta$mZ#/10000
  #
  # roll  = atan2(Sx,(sqrt(Sy^2 + Sz^2)))*(180/pi)
  # pitch = atan2(Sy,(sqrt(Sx^2 + Sz^2)))*(180/pi)
  # yaw   = atan2(Sz,(sqrt(Sx^2 + Sy^2)))*(180/pi)
  #
  # centered_accx = Sx/sqrt(Sx^2 + Sy^2 + Sz^2)
  # centered_accy = Sy/sqrt(Sx^2 + Sy^2 + Sz^2)
  # centered_accz = Sz/sqrt(Sx^2 + Sy^2 + Sz^2)
  #
  # return(list(roll=roll,
  #             pitch = pitch,
  #             yaw = yaw,
  #             centered_accx = centered_accx,
  #             centered_accy = centered_accy,
  #             centered_accz = centered_accz))
}
