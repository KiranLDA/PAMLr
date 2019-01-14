#' Plot Actogram
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param activity Activity data in POSIXct format, most commonly `PAM_data$acceleration$act`
#' @param tz Time zone for POSIXct, default set to "UTC"
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#' @param dt the time interval to which the data are resampled (secs). Default is `NA`
#' @param xlab label for x-axis
#' @param ylab label for y axis
#' @param cex size of labels
#' @param col Colour scheme of plot. Default `col = c("black",viridis::magma(90))`
#' @param ... Any additional parameters used by graphics::image
#'
#' @return an actogram
#' @importFrom graphics image
#' @examples
#' #specify the data location
#' data(hoopoe)
#' PAM_data=hoopoe
#'
#' par(mar=c(4,4,1,6))
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act)
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act, offset=12)
#'
#' @importFrom graphics image mtext axis.POSIXct axis box
#' @importFrom viridis magma viridis
#' @importFrom stats approx
#' @importFrom raster rotate
#'
#' @export
plotACTOGRAM <- function (date, activity , tz="UTC",
                     offset = 0, dt = NA, xlab = "Hour", ylab = "Date", cex=2,
                     col = c("black",viridis::magma(90)),...)
{


  # PAM_data = importPAM("R:/40 Data/20 Geolocator/10 Raw data/UpuEpoCH15/12HM_20160630")#PAMLr::data(PAM_data)
  # date = PAM_data$acceleration$date
  # activity = PAM_data$acceleration$act
  # offset=0

  dts <- c(5, 10, 15, 20, 30, 60, 90, 120, 180, 240, 300, 360,
           400, 480, 540, 600, 720, 900, 960, 1200)

  if (is.na(dt)) {
    dt <- mean(diff(as.numeric(date)))
    dt <- dts[which.min(abs(dt - dts))]
  }

  tmin <- .POSIXct(as.POSIXct(as.Date(date[1])) + offset *  60 * 60, tz)

  if (as.numeric(tmin) > as.numeric(date[1])) tmin <- tmin - 24 * 60 * 60
  tmax <- .POSIXct(as.POSIXct(as.Date(date[length(date)])) +
                     offset * 60 * 60, tz)
  if (as.numeric(tmax) < as.numeric(date[length(date)])) tmax <- tmax + 24 * 60 * 60
  activity <- approx(as.numeric(date), activity, seq(as.numeric(tmin) + dt/2,
                                       as.numeric(tmax) - dt/2, dt))$y
  m <- 24 * 60 * 60/dt
  n <- length(activity)/m
  activity <- matrix(activity, m, n)
  hour <- seq(offset, offset + 24, length = m + 1)
  day <- seq(tmin, tmax, length = n + 1)

  #probably a bit overkill, but not plotting in the needed direction otherwise
  rotate <- function(x) t(apply(x, 2, rev))
  # ploti = as.matrix(raster::flip(raster::flip(t(raster::raster(activity)), 1),1))

  # par(par)
  image( hour, as.numeric(day),rotate(t(activity)),#rotate(rotate(activity)),
        # rotate(ploti),
        col= col,
        axes=F, xlab = "", ylab="", ...)
  mtext(ylab, side=2, line=1.2, cex=cex)
  mtext(xlab, side=1, line=3, cex=cex)
  axis.POSIXct(4, at = seq(tmin,tmax,  length = n / 60),
               labels = as.Date(seq(tmax,tmin,length = n / 60)),
               las=1, cex=cex)
  axis(1, at = seq(0, 48, by = 4), labels = seq(0, 48, by = 4)%%24, cex=cex)
  box()



  # plot attempt with rasterVis
  # rasterVis::levelplot(flip(flip(t(raster(activity)),1),1), col.regions = colours,
  #                      grid.text(x= 0, y=0, seq(0, 48, by = 4)),
  #                                # gp=gpar(cex=0.5), rot=30,
  #                                # default.units='native'),
  #                      # xlab=c('Longitude', 'X-Axis'),
  #                      # ylab=list('Latitude', rot=30, fontface='bold')
  #                      xlab = xlab, ylab=ylab)
  # axis.POSIXct(2, at = seq(tmin,tmax,  length = n / 60), labels = as.Date(seq(tmax,tmin,length = n / 60)),las=1)
  # axis(1, at = seq(0, 48, by = 4), labels = seq(0, 48, by = 4)%%24)

#
#   invisible(list(date = day, hour = hour, offset = offset))
}
