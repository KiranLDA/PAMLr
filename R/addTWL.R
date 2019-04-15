#' Add sunrise sunset to Actogram
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#' @param ... Any additional parameters taken by graphics::points which the user may want to use to modify the graphic
#'
#' @return a plot
#'
#' @examples
#'
#' data(hoopoe)
#'
#' # Calculate twilights
#' twilights <- GeoLight::twilightCalc(hoopoe$light$date, hoopoe$light$obs,
#' LightThreshold = 2, ask = FALSE)
#'
#' par(mar=c(4,4,4,2),mfrow=c(1,2), oma= c(0,0,0,8))
#'
#' # Plot daytime in middle
#' sensorIMG(hoopoe$acceleration$date,
#'           hoopoe$acceleration$act,
#'           main = "Day in middle",ploty=FALSE,
#'           offset=0,
#'           col=c("black",viridis::cividis(90)),
#'           cex=1.2, cex.main = 2)
#' #Add sunrises and sunsets
#' addTWL(twilights$tFirst,
#'        offset=0,
#'        col= ifelse(twilights$type == 1,
#'                    "goldenrod","cornflowerblue"),
#'        pch=16, cex=0.5)
#'
#' # Plot nightime in the middle
#' offset=12
#' sensorIMG(hoopoe$acceleration$date,
#'           hoopoe$acceleration$act,
#'           main = "Night in middle",ploty=TRUE,
#'           offset=offset,
#'           col=c("black",viridis::cividis(90)),
#'           cex=1.2, cex.main = 2)
#'
#' addTWL(twilights$tFirst,
#'        offset=offset,
#'        col= ifelse(twilights$type == 1,
#'                    "goldenrod","cornflowerblue"),
#'        pch=16, cex=0.5)
#'
#'
#'
#' @importFrom graphics points
#'
#' @export
addTWL <- function(date, offset, ...){
  points(hourOffset(as.hour(date), offset%%24), rev(date),  ...)
}

#' Utilities for manipulating hours
#'
#' Given a vector of POSIXct dates, \code{as.hour} extracts the time
#' of day component of the date and returns it as decimal hours.
#' Given a vector of decimal hours, \code{hourOffset} recodes the
#' decimal hour into a new 24 hour interval.
#' @title Hour Manipulation
#' @rdname hours
#' @param tm them timestamp as POSIXct.
#' @param hr the decimal hour to be wrap.
#' @param offset minimum hour of the interval to wrap into.
#' @return Return a decimal hour.
#' @examples
#' as.hour(as.POSIXct("2005-11-12 19:58:00"))
#' hourOffset(1:10,5)
#' @export
as.hour <- function(tm) {
  (as.numeric(tm)-as.numeric(as.POSIXct(as.Date(tm))))/3600
}


#' @rdname hours
#' @export
hourOffset <-  function(hr,offset=0) {
  (hr-offset)%%24+(offset%%24)
}
