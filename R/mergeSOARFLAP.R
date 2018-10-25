#' Compare two different classifications
#'
#' @param dta data stored as a list. see data(PAM_data) then str(PAM_data) for example format
#' @param interpolate can be either TURE or FALSE. Use with caution. If TRUE, then linearly interpolates pressure observations
#' @return merged observations and classifications
#'
#' @importFrom data.table data.table
#'
#' @examples
#' #specify the data location
#' data(PAM_data)
#' str(PAM_data)
#' # at first glance it looks like the logger was removed off a birds and left in arucksack
#' # so remove un-needed data
#' PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#'                                                & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#' PAM_data$pressure = PAM_data$pressure[((PAM_data$acceleration$date >= "2016-07-30")
#'                                                & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#' mergeTBL = mergeSOARFLAP(P = PAM_data$pressure, A=PAM_data$acceleration, interpolate = F)
#' plot(mergeTBL$date[!is.na(mergeTBL$obs)][1500:3500],mergeTBL$obs[!is.na(mergeTBL$obs)][1500:3500],
#'            type="o", pch=16,
#'            col=col[mergeTBL$classSOAR[!is.na(mergeTBL$obs)][1500:3500]])
#' plot(mergeTBL$date[!is.na(mergeTBL$obs)][1500:3500],mergeTBL$obs[!is.na(mergeTBL$obs)][1500:3500],
#'              type="o", pch=16,
#'              col=col[mergeTBL$classFLAP[!is.na(mergeTBL$obs)][1500:3500]])
#'
#' @export
mergeSOARFLAP <- function(P, A, soaring_duration = 2, flapping_duration = 3,interpolate = F){
  ## Create two data.tables with which to demonstrate a data.table merge
  rownames(P) = c()
  rownames(A) = c()

  # get rid of NAs
  P$classSOAR = classifySOAR(dta = P, period = soaring_duration, toPLOT = F)$classification
  A$classFLAP = classifyFLAP(dta = A, period = flapping_duration, toPLOT = F)$classification

  pressure = data.table::data.table(P, key="date")
  activity = data.table::data.table(A, key="date")

  mergeTBL = pressure[activity]

  if (interpolate == T){
    mergeTBL = mergeTBL %>%
      dplyr::mutate(obs.interpolate = zoo::na.approx(mergeTBL$obs)) %>%
      dplyr::mutate(classSOAR.interpolate = round(zoo::na.approx(mergeTBL$classSOAR)))
  }

  return(mergeTBL)
}
