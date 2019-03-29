#' Plot calibrated magnetic data on a sphere
#'
#' @param magX magentic data on x axis
#' @param magY magentic data on y axis
#' @param magZ magentic data on z axis
#' @param col color of points
#'
#' @return a 3d sphere plot
#'
#' @examples
#' data("swift")
#' start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-04-15","%Y-%m-%d", tz="UTC")
#' swift = cutPAM(swift, start, end)
#' PAM_data = swift
#'
#' calibration = triMAG(dta = PAM_data$magnetic)
#'
#' #magSPHERE(magX = calibration$calib_magx,
#' #         magY = calibration$calib_magy,
#' #         magZ = calibration$calib_magz)
#'
#' @importFrom rgl spheres3d abclines3d arrow3d text3d open3d rgl.user2window
#' @importFrom grDevices xyz.coords
#' @export
magSPHERE <- function(magX ,magY ,magZ, col="black" ){
  open3d()
  spheres3d(0,0,0,radius=0.97,lit=FALSE,color="white")
  abclines3d(x = matrix(0, ncol=3), a = diag(3), col="black", lwd=3)
  spheres3d(0,0,0,radius=0.98,lit=FALSE,color="black",front="lines")
  spheres3d(magX, magY, magZ, radius=0.02, col=col)
  arrow3d(p0 = c(-1.5,0,0), p1 = c(1.5,0,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  text3d(x=1.5,y=0.2,z=0.2, texts='x')
  arrow3d(p0 =c(0,-1.5,0), p1 = c(0,1.5,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  text3d(y=1.5,x=0.2,z=0.2, texts='y')
  arrow3d(p0 =c(0,0,-1.5), p1 = c(0,0,1.5), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  text3d(z=1.5,y=0.2,x=0.2, texts='z')
}

