#' Timetable to timeseries
#'
#' @description convert a classification timetable into a classification timeseries
#'
#' @param from start of event that was classified (generally SOARprep output)
#' @param to end of event that was classified (generally SOARprep output)
#' @param classification of event (generally classifyPAM()$states output )
#' @param addTO data which the classifications are to be added to (e.g. PAM_data$pressure)
#' @param missing Missing value replacement. By default NA.
#'
#' @return the classification in addTO dataset
#'
#' @examples
#' #data(bee_eater)
#' #PAM_data = bee_eater
#'
#' #twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#' #                             LightThreshold = 2, ask = FALSE)
#'
#'
#' #TOclassify = pamPREP(PAM_data,
#' #                     method="pressure",
#' #                     twl = twl,
#' #                     Pdiff_thld = 2,
#' #                     light_thld = 2)
#'
#' #classification = classifyPAM((TOclassify$total_daily_duration *
#' #                              log(TOclassify$night_P_diff+0.001 )
#' #                              * TOclassify$total_daily_P_change),
#' #                             states=3, "hmm")$cluster
#'
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #                                             to =TOclassify$end,
#' #                                             classification = classification,
#' #                                             addTO = PAM_data$pressure,
#' #                                             missing = NA)
#'
#' #pressure_classification[pressure_classification == NA] = 0
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     col= viridis::viridis(4)[pressure_classification+1],
#' #     type="o", pch=16, cex=0.6)
#'
#' @importFrom dplyr last
#'
#' @export
classification2PAM <- function(from, to, classification, addTO, missing = NA ){
  print("Error: This function is deprecated, use wrangle_classification, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")

  # addTO$classification  <- missing
  # for(i in 1:length(classification)){
  #   start = which(addTO$date >= from[i])[1]
  #   end = last(which(addTO$date <= to[i]))
  #   addTO$classification[start:end] = classification[i]
  # }
  # return(addTO$classification)
}
