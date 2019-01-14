#' PAMLr
#'
#' This package manipulates data from SOI-GDL3pam loggers (developped by the Swiss Ornithological Institute). These measure Pressure, Activity, Magnetism and Light.
#'
#' @docType package
#'
#' @author Kiran Dhanjal-Adams \email{kiran.dhanjal.adams@gmail.com}
#'
#' @name PAMLr
#' @importFrom data.table data.table
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom dplyr "%>%" distinct
#' @importFrom dygraphs dygraph dyRangeSelector dyHighlight dyLegend dyOptions dyShading "%>%"
#' @importFrom GeoLight twilightCalc
#' @importFrom graphics points image mtext axis.POSIXct axis box hist plot abline text par
#' @importFrom grDevices dev.new rgb
#' @importFrom htmltools browsable tagList
#' @importFrom raster rotate
#' @importFrom stats kmeans aggregate approx gaussian poisson sd complete.cases
#' @importFrom TwGeos hourOffset as.hour
#' @importFrom utils read.delim
#' @importFrom viridis magma viridis
#' @importFrom xts xts
#' @importFrom zoo na.approx
NULL
