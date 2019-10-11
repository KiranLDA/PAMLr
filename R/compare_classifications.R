#' Combine classification results
#'
#' @description this function takes multiple classifications of the same data and compares the agreement between each, on a timepoint level.
#'
#' @param date datetime in POSIXCT format. See hoopoe$activity$date for example
#' @param classifications a dataframe containing the results from different classifications in each column. The classifications must correspond to the same datetimes.
#'
#' @return a dataframe containing date
#' @return the raw classifications
#' @return each state with the number of times it was used in a classification
#' @return whether or not all classes provided the same state
#'
#' @examples
#'
#' # Import data
#' data(hoopoe)
#' start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
#' PAM_data= wrangle_crop(hoopoe,start,end)
#'
#' # perform one classification using classifyFLAP
#' classification = classify_flap(dta = PAM_data$acceleration, period = 12)
#' # Put the classification in the same resolution as pressure
#' class1 = wrangle_classification(from = classification$timetable$start,
#'                             to = classification$timetable$end,
#'                             # because the timetable only contains migration periods
#'                             classification = rep_len(1,length(classification$timetable$end)),
#'                             add_to = PAM_data$pressure)
#' # Convert to categories
#' class1 = ifelse(class1 == classification$migration, "Migration", "Other")
#'
#'
#' # Perform another classification using pressure difference
#' class2 = c(0,ifelse(abs(diff(PAM_data$pressure$obs))>2, "Migration", "Other"))
#'
#' # both classes have been converted to the same time intervals as pressure,
#  # so date should also be the same
#' date = PAM_data$pressure$date
#'
#' # Combine the classifications into a dataframe
#' classifications = data.frame(flap = class1, # flapping classification
#'                             Pdiff = class2) # pressure difference classification
#'
#' class_comparison = compare_classifications(date=date,
#'                                 classifications=classifications)
#'
#' plot(PAM_data$pressure$date,
#'      PAM_data$pressure$obs,
#'      type = "l",
#'      xlab = "Date",
#'      ylab = "Pressure (hPa)",
#'      col = "royalblue3")
#'
#' points(PAM_data$pressure$date,
#'        PAM_data$pressure$obs,
#'        cex = class_comparison$Migration / 2,
#'        col ="orange",
#'        pch = 16)
#'
#' @importFrom stats setNames
#' @export
compare_classifications <- function(date,
                         classifications){

  classes = colnames(classifications)
  states = colnames(table(classifications))

  classifications = cbind(classifications, setNames( lapply(states, function(x) x=NA), states) )

  classifications[,states] = do.call(cbind,
                                     lapply(1:length(states), function(st) unlist(
                                       lapply(1:nrow(classifications), function(r)length(which(
                                         classifications[r, which(colnames(classifications) %in% classes)] == states[st]))
                                       ))))

  classifications$agreement = apply(classifications[,states],1, function(x) any(x == length(classes)))
  return(classifications)
}
