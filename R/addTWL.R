#' Add sunrise sunset to Actogram
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#'
#' @return a plot
#'
#' @examples
#' par(mar=c(4,4,1,6),mfrow=c(1,2))
#' # Calculate twilights
#' twilights <- GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs, LightThreshold = 2, ask = F)
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act)
#' addTWL(twilights$tFirst, col= ifelse(twilights$type == 1,  "goldenrod","cornflowerblue"), pch=16, cex=0.5)
#'
#'
#' offset=12
#' plotACTOGRAM(date = PAM_data$acceleration$date,activity = PAM_data$acceleration$act, offset=offset,col=viridis::cividis(90))
#' addTWL(twilights$tFirst, offset=offset, col= ifelse(twilights$type == 1,  "goldenrod","cornflowerblue"), pch=16, cex=0.5)
#' @export
addTWL <- function (date, offset=0, ...)
{
  points(TwGeos::hourOffset(as.hour(date), offset%%24), rev(date),  ...)
}
