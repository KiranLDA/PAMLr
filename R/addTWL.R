#' Add sunrise sunset to Actogram
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#'
#' @return a plot
#'
#' @examples
#'
#' data(PAM_data)
#'
#'
#' par(mar=c(4,4,1,6),mfrow=c(1,2))
#' # Calculate twilights
#' twilights <- GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#' LightThreshold = 2, ask = F)
#'
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act)
#' addTWL(twilights$tFirst, offset=0, col= ifelse(twilights$type == 1,
#'  "goldenrod","cornflowerblue"), pch=16, cex=0.5)
#'
#'
#' offset=12
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act,
#' offset=offset,col=c("black",viridis::cividis(90)))
#' addTWL(twilights$tFirst, offset=offset, col= ifelse(twilights$type == 1,
#' "goldenrod","cornflowerblue"), pch=16, cex=0.5)
#'
#' @importFrom TwGeos hourOffset
#'
#' @export
addTWL <- function(date, offset, ...){
  points(hourOffset(as.hour(date), offset%%24), rev(date),  ...)
}
