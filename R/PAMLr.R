#' pamlr
#'
#' This package manipulates data from SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute). These measure Pressure, Activity, Magnetism and Light.
#'
#' @keywords internal
#' "_PACKAGE"
#'
#' @author Kiran Dhanjal-Adams \email{kiran.dhanjaladams@uqconnect.edu.au}
#'
#' @name pamlr
#' @importFrom changepoint cpt.mean cpt.var cpt.meanvar cpts
#' @importFrom cluster daisy agnes diana
#' @importFrom data.table data.table
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom dplyr "%>%" distinct last
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom EMbC embc
#' @importFrom graphics points image mtext axis.POSIXct axis box hist plot abline text par identify legend
#' @importFrom grDevices dev.new rgb xyz.coords adjustcolor graphics.off
#' @importFrom htmltools browsable tagList
#' @importFrom lubridate floor_date
#' @importFrom raster rotate
#' @importFrom rgl spheres3d abclines3d arrow3d text3d open3d rgl.user2window plot3d rgl.projection
#' @importFrom stats kmeans aggregate gaussian poisson sd complete.cases median cutree as.formula cutree binomial Gamma inverse.gaussian quasipoisson quasibinomial quasi setNames approx
#' @importFrom utils read.delim
#' @importFrom viridis magma viridis
#' @importFrom xts xts
#' @importFrom zoo na.approx
NULL
