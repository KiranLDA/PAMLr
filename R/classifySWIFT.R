#' Make timetable
#'
#' @param addTO the data that the classification is added to, because the data is derived from pressure, classically this would be data(swift)$pressure
#' @param dta data to be used for the classification, for swifts, this is created by the SOARprep function
#' @param method method for classifying data, currently support "hmm" for hidden markov model and "kmeans" for kmeans clustering
#' @param states number of states to classify the data into
#' @param availavariable currently supports c("light", "pressure", "acceleration"), but will not work without "light" and "pressure"
#' @param ... any additional inputs for depmixs4::depmix or or stats::kmeans functions, depending on method selected
#'
#' @return a timetable for when the species was migrating or not
#'
#' @examples
#' data(swift)
#' PAM_data = swift
#'
#' # have a quick look at the data
#' quickPLOT(PAM_data, measurements=c("light","pressure","acceleration"))
#'
#' # from this, we can tell thatlight stopped recording and that the timeseries needs to be cut
#'
#' # crop the data to get rid of no good periods
#' start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-04-21","%Y-%m-%d", tz="UTC")
#' PAM_data = cutPAM(PAM_data, start, end)
#'
#' backup_options <- options()
#' options(viewer=NULL) # ensure it is viewed in internet browser
#' dygraphPAM(dta = PAM_data) # plot
#' options(backup_options) # restore previous viewer settings
#'
#' # make sure it looks ok
#' quickPLOT(PAM_data, measurements=c("light","pressure","acceleration"))
#'
#' # derive a whole bunch of measures which can be used to classifc the data later
#'
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs, LightThreshold = 2, ask = F)
#'
#' TOclassify = SOARprep(dta = PAM_data,
#'                       availavariable = c("pressure", "acceleration","light"),
#'                       twl=twl, diff_P=15)
#' str(TOclassify)
#' TOclassify = TOclassify[complete.cases(TOclassify),]
#'
#'
#'
#' test = classifySWIFT(addTO = PAM_data$pressure,
#'                      dta = TOclassify,
#'                      method = "hmm", # or kmeans
#'                      states = 3,
#'                      availavariable = c("light", "pressure", "acceleration"))
#' par(mfrow=c(1,1))
#' plot(PAM_data$pressure$date,PAM_data$pressure$obs,
#'      col=viridis::viridis(max(test$classification)+1)[test$classification+1],
#'      type="o",
#'      pch=16, cex=ifelse(test$classification == test$migration, 0.6, 0) )
#'
#' @importFrom stats aggregate kmeans poisson gaussian
#' @importFrom depmixS4 depmix fit posterior
#'
#' @export
classifySWIFT <- function(addTO ,
                          dta ,
                          method = "hmm", # or kmeans
                          states = 2,
                          availavariable = c("light", "pressure", "acceleration"), ... )
{

  if ("acceleration" %in% availavariable ){
    chosen_variable <- dta$night_act_diff * dta$night_P_diff * dta$sd_nextnight_P * dta$sd_night_P # * dta$total_daily_duration#* dta$sum_activity #* dta$pressure_change * dta$pressure_range
     }else{
    chosen_variable <-dta$night_P_diff * dta$sd_nextnight_P * dta$sd_night_P#* dta$pressure_change * dta$pressure_range
   }
  classification <- classifyPAM(chosen_variable, states= states, method=method, ...)

  cluster = aggregate(chosen_variable, list(classification$cluster), mean)

  class = as.numeric(sort(cluster$x))
  if(states == 2){
    low_movement = which(cluster$x == class[1])
    high_movement = which(cluster$x == class[1])
  }
  if (states >= 3){
    low_movement = which(cluster$x == class[length(class)-2])
    high_movement = which(cluster$x == class[length(class)-1])
  }

  migration = which(cluster$x == max(class))
  index = which(classification$state == migration)

  Duration_table = data.frame(dta$start[index],dta$end[index],dta$duration[index])
  names(Duration_table) = c("start","end","Duration (h)")


  clust = classification2PAM(from = dta$start,
                             to = dta$end,
                             classification = classification$cluster,
                             addTO = addTO)

  print("Please note that this function is under development")
  return(list(type = "swift",
              timetable = Duration_table,
              classification = clust,
              low_movement = low_movement,
              high_movement = high_movement,
              migration = migration,
              no_movement = 0,
              threshold = NA))

}
