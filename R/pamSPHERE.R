#' Plot 3d sphere
#'
#' @description This function takes a spherical projection of acceleration or magentic data and plots it on a sphere which can be turned and zoomed into.
#'
#' @param x data on x axis
#' @param y data on y axis
#' @param z data on z axis
#' @param spherecolor color of the sphere
#' @param linecolor color of the lines on the sphere
#' @param linewidth width of the lines on the sphere
#' @param ptcol color of points
#' @param ptsize size of the points plotted on the sphere
#' @param arrows default TRUE - whether or not to draw the arrows
#' @param cex size of font. If cex = 2 then fond is two times bigger.
#' @param ... additional input for rgl::spheres3d for plotting points on the sphere using
#'
#' @return a 3d sphere plot
#'
#' @references Adler, D., Nenadic, O. and Zucchini, W., 2003, March. Rgl: A r-library for 3d visualization with opengl. In Proceedings of the 35th Symposium of the Interface: Computing Science and Statistics, Salt Lake City (Vol. 35).
#'
#' @examples
#' data("swift")
#' start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-04-15","%Y-%m-%d", tz="UTC")
#' swift = cutPAM(swift, start, end)
#' PAM_data = swift
#'
#' # plot an m-phere
#' calibration = triMAG(dta = PAM_data$magnetic)
#' pamSPHERE(x = calibration$calib_magx,
#'           y = calibration$calib_magy,
#'           z = calibration$calib_magz,
#'           ptcol = "goldenrod",
#'           ptsize = 0.03,
#'           linecolor ="black",
#'           spherecolor="royalblue4",
#'           arrows=TRUE,
#'           cex=2)
#'
#' # plot an g-phere
#' calibration = triACC(dta = PAM_data$magnetic)
#' pamSPHERE(x = calibration$centered_accx,
#'           y = calibration$centered_accy,
#'           z = calibration$centered_accz,
#'           ptcol = "royalblue4",
#'           ptsize = 0.03,
#'           linecolor ="black",
#'           spherecolor="goldenrod",
#'           arrows=TRUE)
#'
#' @importFrom rgl spheres3d abclines3d arrow3d text3d open3d rgl.user2window rgl.projection
#' @importFrom grDevices xyz.coords adjustcolor
#' @export
pamSPHERE <- function(x ,y ,z,
                      spherecolor = "white",
                      linecolor = "black",
                      linewidth = 2,
                      ptcol="black",
                      ptsize = 0.02,
                      arrows= TRUE,
                      cex = 1.5,
                      ...){
  open3d()
  spheres3d(0,0,0,radius=0.97,lit=FALSE,color=spherecolor)
  # abclines3d(x = matrix(0, ncol=3), a = diag(3), col=linecolor, lwd=linewidth)
  spheres3d(0,0,0,radius=0.98,lit=FALSE,color=linecolor,front="lines")
  spheres3d(x, y, z, radius=ptsize, col=ptcol,...)
  if(arrows == TRUE){
    arrow3d(p0 = c(-1.5,0,0), p1 = c(1.5,0,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
    text3d(x=1.7,y=0,z=0, texts='x', cex=cex)
    arrow3d(p0 =c(0,-1.5,0), p1 = c(0,1.5,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
    text3d(y=1.7,x=0,z=0, texts='y', cex=cex)
    arrow3d(p0 =c(0,0,-1.5), p1 = c(0,0,1.5), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
    text3d(z=1.7,y=0,x=0, texts='z', cex=cex)
  }

  # open3d()
  # spheres3d(0,0,0,radius=0.97,lit=FALSE,color=spherecolor)
  # # abclines3d(x = matrix(0, ncol=3), a = diag(3), col=linecolor, lwd=linewidth)
  # spheres3d(0,0,0,radius=0.98,lit=FALSE,color=linecolor,front="lines")
  # spheres3d(x=y,y=x, z=z, radius=ptsize, col=ptcol,...)
  # if(arrows == TRUE){
  #   arrow3d(p0 = c(-1.5,0,0), p1 = c(1.5,0,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  #   text3d(x=1.7,y=0,z=0, texts='y', cex=cex)
  #   arrow3d(p0 =c(0,-1.5,0), p1 = c(0,1.5,0), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  #   text3d(y=1.7,x=0,z=0, texts='x', cex=cex)
  #   arrow3d(p0 =c(0,0,-1.5), p1 = c(0,0,1.5), barblen=.06, s=1/7, lwd=0.08, type = "rotation")
  #   text3d(z=1.7,y=0,x=0, texts='z', cex=cex)
  # }
}

