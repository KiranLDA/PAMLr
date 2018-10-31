#' Plot threshold
#'
#' @param dta Raw acceleration or pressure data used to make the classification
#' @param type The type of classification used i.e "flapping" or soar-gliding
#' @param classification Is the result of the classification. is in the format of a vector of numbers used for low or high activity
#' @param threshold The threshold between different classes
#'
#' @return a graphic with the output from the classifications
#'
#' @examples
#' #specify the data location
#' data(PAM_data)
#'
#' PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#' & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#' behaviour = classifyFLAP(dta = PAM_data$acceleration, flapping_duration = 3, toPLOT = F)
#'
#' plotTHLD(dta = PAM_data$acceleration$act,
#'     type = behaviour$type,
#'      classification = behaviour$classification,
#'      threshold = behaviour$threshold)
#'
#' @export
plotTHLD <- function(dta , classification, threshold, type ){
  if (type == "flapping"){
  dev.new()
  par(mar=c(4,4,1,1))
  hist(dta[dta != 0],
       breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
       xlab="Activity",
       main ="Initial High / Low activity Classification", border=F)
  plot(hist(dta[classification==1 & dta != 0],
            breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
            plot=F),col = rgb(1,0,0,1/4), add=T, border=F)
  plot(hist(dta[classification==2 & dta != 0],
                                breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot=F),
                 col = rgb(0,0,0,1/4), add=T, border=F)
  abline(v=threshold, lty=2)
  text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                  plot=F)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                               plot=F)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "pressure_change"){
    dev.new()
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
         xlab="Pressure difference",
         main =" High / Low night time pressure change Classification", border=F)
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
              plot=F),col = rgb(1,0,0,1/4), add=T, border=F)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot=F),
         col = rgb(0,0,0,1/4), add=T, border=F)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot=F)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot=F)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
  if (type == "soarglide"){
    dev.new()
    par(mar=c(4,4,1,1))
    hist(dta[dta != 0],
         breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
         xlab="Pressure difference",
         main ="Initial High / Low activity Classification", border=F, xlim=c(0,15))
    plot(hist(dta[classification==1 & dta != 0],
              breaks = (max(dta[classification==1 & dta != 0])-min(dta[classification==1 & dta != 0])),
              plot=F),col = rgb(1,0,0,1/4), add=T, border=F)
    plot(hist(dta[classification==2 & dta != 0],
              breaks = (max(dta[classification==2 & dta != 0])-min(dta[classification==2 & dta != 0])), plot=F),
         col = rgb(0,0,0,1/4), add=T, border=F)
    abline(v=threshold, lty=2)
    text(x = threshold,  (max((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                    plot=F)$counts)) - min((hist(dta[dta != 0],breaks = (max(dta[dta != 0])-min(dta[dta != 0])),
                                                                 plot=F)$counts)))/2, paste0("threshold = ",threshold), pos=4)
  }
}
