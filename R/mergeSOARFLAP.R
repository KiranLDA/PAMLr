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
#' mergeTBL = mergeSOARFLAP(dta = PAM_data, interpolate = F)
#' plot(mergeTBL$date[!is.na(mergeTBL$obs)][500:1500],mergeTBL$obs[!is.na(mergeTBL$obs)][500:1500],
#'            type="o", pch=16,
#'            col=col[mergeTBL$classSOAR[!is.na(mergeTBL$obs)][500:1500]])
#' plot(mergeTBL$date[!is.na(mergeTBL$obs)][500:1500],mergeTBL$obs[!is.na(mergeTBL$obs)][500:1500],
#'              type="o", pch=16,
#'              col=col[mergeTBL$classFLAP[!is.na(mergeTBL$obs)][500:1500]])
#'
#' @export
mergeSOARFLAP <- function(dta = PAM_data, interpolate = F){
  ## Create two data.tables with which to demonstrate a data.table merge
  pressure = data.table(dta$pressure, key="date")
  activity = data.table(dta$acceleration, key="date")
  pressure$classSOAR = classifySOAR(dta = PAM_data$pressure, soaring_duration = 2, toPLOT = F)$classification
  activity$classFLAP = classifyFLAP(dta = PAM_data$acceleration, flapping_duration = 3, toPLOT = F)$classification

  mergeTBL = pressure[activity]

  if (interpolate == T){
    mergeTBL = mergeTBL %>%
      mutate(obs.interpolate = na.approx(obs)) %>%
      mutate(classSOAR.interpolate = round(na.approx(classSOAR)))
  }
}
