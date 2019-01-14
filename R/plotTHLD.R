#' Plot threshold
#'
#' @param dta Raw acceleration or pressure data used to make the classification
#' @param type The type of classification used i.e "flapping" or soar-gliding
#' @param classification Is the result of the classification. is in the format of a vector of numbers used for low or high activity
#' @param threshold The threshold between different classes
#' @param new_window whether you want to plot it in a new window or not
#' @param ... any additional parameters used by graphics::hist
#'
#' @return a graphic with the output from the classifications
#'
#' @examples
#' #specify the data location
#' data(hoopoe)
#' PAM_data=hoopoe
#'
#' PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#' & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#' behaviour = classifyFLAP(dta = PAM_data$acceleration,
#'                          period = 3,
#'                          toPLOT = FALSE)
#'
#' plotTHLD(dta = PAM_data$acceleration$act,
#'          type = behaviour$type,
#'          classification = behaviour$classification,
#'          threshold = behaviour$threshold)
#'
#' @importFrom graphics hist plot abline text par
#' @importFrom grDevices dev.new rgb
#'
#' @export
plotTHLD <- function(dta , classification, threshold, type, new_window = TRUE, ...){
  if(new_window == TRUE) dev.new()
  if (type == "flapping"){
  par(mar=c(4,4,1,1))
  hist(dta[dta != 0],
       breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
       xlab="Activity",
       main ="Initial High / Low activity Classification", border = FALSE, ...)
  plot(hist(dta[classification==1 & dta != 0],
            breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
            plot = FALSE),col = rgb(1,0,0,1/4), add = TRUE, border = FALSE)
  plot(hist(dta[classification==2 & dta != 0],
                                breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot = FALSE),
                 col = rgb(0,0,0,1/4), add = TRUE, border = FALSE)
  abline(v=threshold, lty=2)
  text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                  plot = FALSE)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                               plot = FALSE)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "light"){
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0]))*2,
         xlab="Duration",
         main ="Short/Long flight duration Classification", border = FALSE, ...)
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0]))*2,
              plot = FALSE),col = rgb(1,0,0,1/4), add = TRUE, border = FALSE)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0]))*2, plot = FALSE),
         col = rgb(0,0,0,1/4), add = TRUE, border = FALSE)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot = FALSE)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot = FALSE)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "pressure_change"){
    # dev.new()
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
         xlab="Pressure difference",
         main =" High / Low night time pressure change Classification", border = FALSE, ...)
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
              plot = FALSE),col = rgb(1,0,0,1/4), add = TRUE, border = FALSE)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot = FALSE),
         col = rgb(0,0,0,1/4), add = TRUE, border = FALSE)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot = FALSE)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot = FALSE)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "soarglide"){
    # dev.new()
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
         xlab="Pressure difference",
         main ="Initial High / Low activity Classification", border = FALSE, xlim=c(0,15), ...)
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
              plot = FALSE),col = rgb(1,0,0,1/4), add = TRUE, border = FALSE)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot = FALSE),
         col = rgb(0,0,0,1/4), add = TRUE, border = FALSE)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot = FALSE)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot = FALSE)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "flighthours"){
    # dev.new()
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
         xlab="time spent flying",
         main ="Classification", border = FALSE, xlim=c(0,15), ...)
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
              plot = FALSE),col = rgb(1,0,0,1/4), add = TRUE, border = FALSE)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot = FALSE),
         col = rgb(0,0,0,1/4), add = TRUE, border = FALSE)
    plot(hist(dta[classification==3 & dta != 0],
              breaks = (max(dta[classification==3 & dta != 0])-min(dta[classification==3 & dta != 0])), plot = FALSE),
         col = rgb(1/2,0,0,1/4), add = TRUE, border = FALSE)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot = FALSE)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot = FALSE)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
}
