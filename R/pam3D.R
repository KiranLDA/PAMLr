#' 3d scatterplot
#'
#' @description Creates an interactive 3d scatterplot
#'
#' @param x data to plot on x axis
#' @param y data to plot on y axis
#' @param z data to plot on z axis
#' @param ... any additional parameters used by rgl::plot3d
#'
#' @return a 3d scatter plot
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
#' calibration = triMAG(dta = PAM_data$magnetic)
#'
#' pam3D(PAM_data$magnetic$mX, PAM_data$magnetic$mY, PAM_data$magnetic$mZ,
#'        xlab= "X", ylab= "Y", zlab= "Z")
#'
#' @importFrom rgl plot3d
#' @export
pam3D <- function(x ,y, z , ... ){
 rgl::plot3d(x, y, z, ...)
}
