#' Convert data from a Timetable to a timeseries
#'
#' @description convert a classification timetable into a classification timeseries
#'
#' @param from start of event that was classified (generally timetable output)
#' @param to end of event that was classified (generally timetable output)
#' @param classification classified data
#' @param add_to data which the classifications are to be added to (e.g. PAM_data$pressure)
#' @param missing Missing value replacement. By default NA.
#'
#' @return the classification in add_to dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#'
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#'                              LightThreshold = 2, ask = FALSE)
#'
#'
#' to_classify = create_summary_statistics(PAM_data,
#'                      method="pressure",
#'                      twl = twl,
#'                      Pdiff_thld = 2,
#'                      light_thld = 2)
#'
#' classification = classify_summary_statistics((to_classify$total_daily_duration *
#'                               log(to_classify$night_P_diff+0.001 )
#'                               * to_classify$total_daily_P_change),
#'                              states=3, "hmm")$cluster
#'
#' pressure_classification = create_merged_classification(from = to_classify$start,
#'                                                        to =to_classify$end,
#'                                                        classification = classification,
#'                                                        add_to = PAM_data$pressure,
#'                                                        missing = NA)
#'
#' pressure_classification[pressure_classification == NA] = 0
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      col= viridis::viridis(4)[pressure_classification+1],
#'      type="o", pch=16, cex=0.6)
#'
#' @importFrom dplyr last
#'
#' @export
create_merged_classification <- function(from, to, classification, add_to, missing = NA ){
  add_to$classification  <- missing
  for(i in 1:length(classification)){
    start = which(add_to$date >= from[i])[1]
    end = last(which(add_to$date <= to[i]))
    add_to$classification[start:end] = classification[i]
  }
  return(add_to$classification)
}
