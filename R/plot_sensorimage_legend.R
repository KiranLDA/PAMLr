#' sensor image legend
#'
#' @description This function adds a legend to a sensor image
#'
#' @param dta data to be plotted (for instance temperature or pressure)
#' @param col_palette color palette to use in the graphic
#' @param inset where to place the legend default is at the bottom -0.12
#' @param ncol number of columns in legend, default is 5
#' @param bg whether or not to have a background, default is NA
#' @param cex size of labels
#' @param ... Any additional parameters used by graphics::legend
#'
#' @return an image of the sensor data, for instance with activity it would produce an actogram
#'
#' @importFrom graphics image
#'
#' @examples
#' #specify the data location
#' data(hoopoe)
#' start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
#' PAM_data = create_crop(hoopoe,start,end)
#'
#' # make margins big so that there is enough space for margin
#' par( mfrow= c(1,1), oma=c(4,2,0,6))
#'
#' col_palette = c("black",viridis::cividis(90))
#' par(mar =  c(4,2,4,2))
#' plot_sensorimage(PAM_data$acceleration$date,
#'           PAM_data$acceleration$act, main = "Activity",
#'           col=col_palette, cex=1.2, cex.main = 2)
#' plot_sensorimage_legend(PAM_data$acceleration$act, col_palette)
#'
#' @importFrom graphics legend
#'
#' @export
plot_sensorimage_legend <- function(dta, col_palette, inset=c(0.5,-0.12), cex=1, ncol=5, bg=NA, ...){
  scaLAb = max(dta, na.rm=TRUE) - min(dta, na.rm=TRUE)
  minLab = min(dta, na.rm=TRUE)
  legend("bottom",
         inset=inset, cex=cex, ncol=ncol,bg=bg,
         legend = c(round(minLab), round(minLab+scaLAb*0.25),
                    round(minLab+scaLAb*0.50),
                    round(minLab+scaLAb*0.75),
                    round(minLab+scaLAb)),
         fill = col_palette[c(1, length(col_palette)*0.25,
                         length(col_palette)*0.50,
                         length(col_palette)*0.75,
                         length(col_palette))], xpd = NA,
         ...)

}


