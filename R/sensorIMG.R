#' sensor image
#'
#' @description This function plots sensor data as an image. An actogram for instance is a type of sensor image.
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param sensor_data sensor data, for example look at `PAM_data$acceleration$act`
#' @param tz Time zone for POSIXct, default set to "UTC"
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#' @param dt the time interval to which the data are resampled (secs). Default is `NA`
#' @param xlab label for x-axis (as a character string)
#' @param ylab label for y axis (as a character string)
#' @param plotx wherether or not to plot the x axis ticks + labels (for instance when compiling multifigures)
#' @param ploty wherether or not to plot the y axis ticks + labels (for instance when compiling multifigures)
#' @param labelx wherether or not to write the name of the x axis (for instance when compiling multifigures)
#' @param labely wherether or not to write the name of the y axis (for instance when compiling multifigures)
#' @param cex size of labels
#' @param col Colour scheme of plot. Default `col = c("black",viridis::magma(90))`
#' @param ... Any additional parameters used by graphics::image
#'
#' @return an image of the sensor data, for instance with activity it would produce an actogram
#'
#' @importFrom graphics image
#'
#' @examples
#' ##specify the data location
#' #data(hoopoe)
#' #start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
#' #end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
#' #PAM_data = cutPAM(hoopoe,start,end)
#'
#' ## Create plots with 3 together (mfrow)
#' #par( mfrow= c(1,3), oma=c(0,2,0,6))
#'
#' #par(mar =  c(4,2,4,2))
#' #sensorIMG(PAM_data$acceleration$date, ploty=FALSE,
#' #          PAM_data$acceleration$act, main = "Activity",
#' #          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
#'
#' #par(mar =  c(4,2,4,2))
#' #sensorIMG(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
#' #          PAM_data$pressure$obs,  main="Pressure",
#' #          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
#'
#' #par(mar =  c(4,2,4,2))
#' #sensorIMG(PAM_data$temperature$date, labely=FALSE,
#' #          PAM_data$temperature$obs,  main="Temperature",
#' #          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
#'
#' ######################################################
#' # Look at a classification output
#' ######################################################
#'
#' ## Classification
#' #classification  =  classifyFLAP(dta = PAM_data$acceleration, period = 10, toPLOT=FALSE)
#'
#' #par( mfrow= c(1,3), oma=c(0,2,0,6),mar =  c(4,2,4,2))
#'
#' #sensorIMG(PAM_data$pressure$date, c(0,abs(diff(PAM_data$pressure$obs))),
#' #          main="Pressure  difference",
#' #          ploty=FALSE,
#' #          col=c("black",viridis::cividis(90)), cex=1.2, cex.main = 2)
#'
#' #sensorIMG(PAM_data$acceleration$date, PAM_data$acceleration$act,  main="Activity",
#' #          ploty=FALSE, labely=FALSE,
#' #          col=c(viridis::cividis(90)), cex=1.2, cex.main = 2)
#'
#' #sensorIMG(PAM_data$acceleration$date,
#' #          ifelse(classification$classification == classification$migration, 1,2),
#' #          main="Migration Classification",
#' #          labely=FALSE,
#' #          col = c("orange","black"),
#' #          cex=1.2, cex.main = 2)
#'
#'
#' #twilights <- GeoLight::twilightCalc(PAM_data$light$date,
#' #                                    PAM_data$light$obs,
#' #                                    LightThreshold = 2,
#' #                                    ask = FALSE)
#'
#' #addTWL(twilights$tFirst, offset=0,
#' #       col= ifelse(twilights$type == 1,
#' #                   "goldenrod","cornflowerblue"),
#' #       pch=16, cex=0.5)
#'
#'
#' @importFrom graphics image mtext axis.POSIXct axis box
#' @importFrom viridis magma viridis
#' @importFrom stats approx
#' @importFrom raster rotate
#'
#' @export
sensorIMG  <- function (date, sensor_data , tz="UTC", plotx=TRUE, ploty=TRUE,
                        labelx = TRUE, labely=TRUE,
                      offset = 0, dt = NA, xlab = "Hour", ylab = "Date", cex=2,
                      col = c("black",viridis::magma(90)), ...) {
  print("Error: This function is deprecated, use plot_sensorimage in latest version of PAMLr which can be installed by running devtools::install_github('KiranLDA/PAMLr')")


#
#   # PAM_data = importPAM("R:/40 Data/20 Geolocator/10 Raw data/UpuEpoCH15/12HM_20160630")#PAMLr::data(PAM_data)
#   # date = PAM_data$acceleration$date
#   # sensor_data = PAM_data$acceleration$act
#   # offset=0
#
#   dts <- c(5, 10, 15, 20, 30, 60, 90, 120, 180, 240, 300, 360,
#            400, 480, 540, 600, 720, 900, 960, 1200)
#
#   if (is.na(dt)) {
#     dt <- mean(diff(as.numeric(date)))
#     dt <- dts[which.min(abs(dt - dts))]
#   }
#
#   tmin <- .POSIXct(as.POSIXct(as.Date(date[1])) + offset *  60 * 60, tz)
#
#   if (as.numeric(tmin) > as.numeric(date[1])) tmin <- tmin - 24 * 60 * 60
#   tmax <- .POSIXct(as.POSIXct(as.Date(date[length(date)])) +
#                      offset * 60 * 60, tz)
#   if (as.numeric(tmax) < as.numeric(date[length(date)])) tmax <- tmax + 24 * 60 * 60
#   sensor_data <- approx(as.numeric(date), sensor_data, seq(as.numeric(tmin) + dt/2,
#                                                      as.numeric(tmax) - dt/2, dt))$y
#   m <- 24 * 60 * 60/dt
#   n <- length(sensor_data)/m
#   sensor_data <- matrix(sensor_data, m, n)
#   hour <- seq(offset, offset + 24, length = m + 1)
#   day <- seq(tmin, tmax, length = n + 1)
#
#   #probably a bit overkill, but not plotting in the needed direction otherwise
#   rotate <- function(x) t(apply(x, 2, rev))
#   # ploti = as.matrix(raster::flip(raster::flip(t(raster::raster(sensor_data)), 1),1))
#
#   # par(par)
#   image( hour, as.numeric(day),rotate(t(sensor_data)),
#          col= col,
#          axes=F,
#          xlab = "", ylab="", ...)
#
#   if(plotx){
#     axis(1, at = seq(0, 48, by = 4),
#          labels = seq(0, 48, by = 4)%%24 ,
#          cex.axis = cex)
#
#   }
#
#   if(labelx == TRUE) mtext(xlab, side=1, line=3, cex=cex)
#
#   if(ploty){
#     axis.POSIXct(4, at = seq(tmin,tmax,  length = n / 60),
#                  labels = as.Date(seq(tmax,tmin,length = n / 60)),
#                  las=1, cex.axis=cex)
#
#   }
#   if(labely == TRUE) mtext(ylab, side=2, line=1.2, cex=cex)
#
#   box()

}


