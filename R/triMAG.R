#' Calibrate magnetic data
#'
#' @description This function calibrates tri-axial magnetic data and then also calculates yaw, pitch and roll
#'
#' @param dta magentic data from PAM logger
#'
#' @return roll, pitch and yaw from acceleration data, as well as calibrated magnetic data for x, y and z axes
#'
#' @references Bidder, O.R., Walker, J.S., Jones, M.W., Holton, M.D., Urge, P., Scantlebury, D.M., Marks, N.J., Magowan, E.A., Maguire, I.E. and Wilson, R.P., 2015. Step by step: reconstruction of terrestrial animal movement paths by dead-reckoning. Movement ecology, 3(1), p.23.
#'
#' @examples
#' #data(swift)
#' #PAM_data = swift
#'
#' #calibration = triMAG(dta = PAM_data$magnetic)
#'
#' @export
triMAG <- function(dta){
  print("Error: This function is deprecated, use calculate_triaxial_magnetic, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")

#
#   # acceleration conversion
#   Sx = dta$mX#/10000
#   Sy = dta$mY#/10000
#   Sz = dta$mZ#/10000
#
#   roll  = atan2(Sx,(sqrt(Sy^2 + Sz^2)))*(180/pi)
#   pitch = atan2(Sy,(sqrt(Sx^2 + Sz^2)))*(180/pi)
#   yaw   = atan2(Sz,(sqrt(Sx^2 + Sy^2)))*(180/pi)
#
#
#   # Calculate the offset "hard distortion"
#   Ox = (max(dta$gX * 0.00016 , na.rm=T) - min(dta$gX * 0.00016, na.rm=T))/2
#   Oy = (max(dta$gY * 0.00016, na.rm=T) - min(dta$gY * 0.00016, na.rm=T))/2
#   Oz = (max(dta$gZ * 0.00016, na.rm=T) - min(dta$gZ * 0.00016, na.rm=T))/2
#
#
#   # correct  the magnetometer output by the offset
#   Mx = (dta$gX * 0.00016 ) - Ox
#   My = (dta$gY * 0.00016 ) - Oy
#   Mz = (dta$gZ * 0.00016 ) - Oz
#
#
#   # normalise the compass using the normalising factor
#   fm = sqrt( Mx^2 + My^2 + Mz ^2)
#
#   NMx = Mx/fm
#   NMy = My/fm
#   NMz = Mz/fm
#
#
#   # Rotate axes according to pitch and roll
#   RNMx = NMx
#   RNMy = NMx
#   RNMz = NMx
#
#   for (i in 1:length(roll)){
#     Rx = matrix(c(1,0,0,
#                   0, cos(roll[i]), -sin(roll[i]),
#                   0,sin(roll[i]),cos(roll[i])),
#                 nrow=3)
#     Ry = matrix(c(cos(pitch[i]), 0, sin(pitch[i]),
#                   0,1,0,
#                   -sin(pitch[i]),0, cos(pitch[i])),
#                 ncol=3)
#     NM = c(NMx[i],NMy[i],NMz[i])
#     RNM = NM %*% (Rx %*% Ry)
#     RNMx[i] = RNM[1]
#     RNMy[i] = RNM[2]
#     RNMz[i] = RNM[3]
#   }
#
#   return(list(roll = roll,
#               pitch = pitch,
#               yaw = yaw,
#               calib_magx = RNMx,
#               calib_magy = RNMy,
#               calib_magz = RNMz)
#          )

}
