#' Add sunrise sunset to Actogram
#'
#' @param date Date data in POSIXct format, most commonly `PAM_data$acceleration$date`
#' @param offset This parameter determines where the center of the graph is. When `offset = 0`, then midday is at the center of the graph. when `offset=12` midnight`
#' @param ... Any additional parameters taken by graphics::points which the user may want to use to modify the graphic
#'
#' @return a plot
#'
#'
#'
#'
#' @importFrom graphics points
#'
#' @export
addTWL <- function(date, offset, ...){
  print("Error: This function is deprecated, use image_add_twilight, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")
  # points(hourOffset(as.hour(date), offset%%24), rev(date),  ...)
}

