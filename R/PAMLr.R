#' PAMLr
#'
#' This package manipulates data from SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute). These measure Pressure, Activity, Magnetism and Light.
#'
#' @docType package
#'
#' @author Kiran Dhanjal-Adams \email{kiran.dhanjal.adams@gmail.com}
#'
#' @name PAMLr
#' @importFrom changepoint cpt.mean cpt.var cpt.meanvar cpts
#' @importFrom cluster daisy agnes diana
#' @importFrom data.table data.table
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom dplyr "%>%" distinct last
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom EMbC embc
#' @importFrom GeoLight twilightCalc
#' @importFrom graphics points image mtext axis.POSIXct axis box hist plot abline text par
#' @importFrom grDevices dev.new rgb xyz.coords adjustcolor
#' @importFrom htmltools browsable tagList
#' @importFrom raster rotate
#' @importFrom rgl spheres3d abclines3d arrow3d text3d open3d rgl.user2window plot3d rgl.projection
#' @importFrom stats kmeans aggregate approx gaussian poisson sd complete.cases median cutree as.formula cutree binomial Gamma inverse.gaussian quasipoisson quasibinomial quasi
#' @importFrom TwGeos hourOffset as.hour
#' @importFrom utils read.delim
#' @importFrom viridis magma viridis
#' @importFrom xts xts
#' @importFrom zoo na.approx
NULL
