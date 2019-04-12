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
#'
#' par(mar=c(4,4,1,6),mfrow=c(1,2))
#' # Calculate twilights
#' twilights <- GeoLight::twilightCalc(hoopoe$light$date, hoopoe$light$obs,
#' LightThreshold = 2, ask = FALSE)
#'
#' sensorIMG(hoopoe$acceleration$date,
#'           hoopoe$acceleration$act,
#'           main = "Activity",ploty=FALSE,
#'           offset=0,
#'           col=c("black",viridis::cividis(90)),
#'           cex=1.2, cex.main = 2)
#'
#'# plotACTOGRAM(date = hoopoe$acceleration$date,activity = hoopoe$acceleration$act)
#' addTWL(twilights$tFirst,
#'        offset=0,
#'        col= ifelse(twilights$type == 1,
#'                    "goldenrod","cornflowerblue"),
#'        pch=16, cex=0.5)
#'
#'
#' offset=12
#' sensorIMG(hoopoe$acceleration$date,
#'           hoopoe$acceleration$act,
#'           main = "Activity",ploty=FALSE,
#'           offset=offset,
#'           col=c("black",viridis::cividis(90)),
#'           cex=1.2, cex.main = 2)
#'
#'# plotACTOGRAM(date = hoopoe$acceleration$date,activity = hoopoe$acceleration$act)
#' addTWL(twilights$tFirst,
#'        offset=offset,
#'        col= ifelse(twilights$type == 1,
#'                    "goldenrod","cornflowerblue"),
#'        pch=16, cex=0.5)
#'
#'
#'
#' @importFrom TwGeos hourOffset as.hour
#' @importFrom graphics points
#'
#' @export
addTWL <- function(date, offset, ...){
  points(hourOffset(as.hour(date), offset%%24), rev(date),  ...)
}
