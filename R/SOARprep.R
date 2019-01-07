#' Derive classification data for soaring birds
#'
#' @param dta PAM data to be used in the analysis
#' @param availavariable Variables to be used to derive metrics for classification. must have "pressure", but ideally `availavariable = c("pressure", "light", "acceleration")` if any of these are incomplete, do not use them
#' @param diff_P pressure threshold. This if pressure changes more than e.g. 2hpa over 30 minutes, then the bird is flying
#' @param twl twilight estimates formatted according to twilightCalc in GeoLight package
#' @param tz Timeuzone. default is "UTC"
#'
#' @return a dataframe of derives metrhics based on pressure: `date`,`start` of flight event,`end`,`duration` of flight event,`total_daily_duration` spent flying that day,`total_daily_flight_number` total number of flight events that day,`pressure_change` absolute total pressure differences during that flight,`P_dep_arr` difference in pressure between the start of the flight and the end of the flight,`pressure_range` difference between min and max pressure during that flight,`mean_night_P` difference in mean pressure from the night before and the night after the flight event,`sd_night_P`,`mean_nextnight_P`,`sd_nextnight_P`,`night_P_diff`,`sum_activity` total amount of activity during that flight event,`prop_resting` proportion of time spent resting during that flight,`prop_active` proportion of time spent active during that flight event
#'
#' @examples
#' data(PAM_data)
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs, LightThreshold = 2, ask = F)
#' availavariable = c("pressure", "light", "acceleration")
#'
#' TOclassify = SOARprep(dta = PAM_data,
#'                       availavariable = c("pressure", "acceleration", "light"),
#'                       twl = twl)
#'
#'
#' @importFrom stats aggregate
#' @import data.table
#' @importFrom GeoLight twilightCalc
#'
#' @export
SOARprep <- function(dta,
                     availavariable = c("pressure", "light", "acceleration"),
                     diff_P = 2,
                     twl,
                     tz="UTC"){


  # # for testing
  # dta <- PAM_data
  # availavariable <- c("pressure", "light", "acceleration")
  # diff_P <- 2 # pressure threshold
  # tz<-"UTC"
  #

  if("pressure" %in% availavariable){
    pressure <- dta$pressure
  }

  if("light" %in% availavariable){
    light <- dta$light
  }

  if("acceleration" %in% availavariable){
    activity <- dta$acceleration
  }


  # find when a bird is flying
  pressure$diff_P <- c(0,abs(diff(pressure$obs)))
  pressure$flying <- 0
  pressure$flying[pressure$diff_P >= diff_P] <- 1

  # find start of flight event
  x <- c(0,1)
  start <- which(pressure$flying == x[1])
  start <- start[sapply(start, function(i) all(pressure$flying[i:(i+(length(x)-1))] == x))]
  start <- start

  # find end of flight event
  x <- c(1, 0)
  end <- which(pressure$flying == x[1])
  end <- end[sapply(end, function(i) all(pressure$flying[i:(i+(length(x)-1))] == x))]

  #remove any back to back start end start end and merge them
  to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
  end <- end[-to_remove]
  start <- start[-(to_remove+1)]

  #housekeeping
  if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end)>length(start)) start <- start[1:length(end)]
  if (length(end)<length(start)) end <- end[1:length(start)]

  # get rif of any 30 minute flights
  index <- which((end-start) > 1)
  start <- start[index]
  end <- end[index]

  #--------------------------------------------------
  #For each flight event, figure out how long it lasted
  flight_list <- data.frame(start = pressure$date[start],
                           end = pressure$date[end],
                           duration = difftime(pressure$date[end], pressure$date[start], tz= tz,
                                               units = "hours"))
  flight_list$duration <- as.numeric(flight_list$duration )
  flight_list$date <- as.Date(flight_list$start)


  #--------------------------------------------------
  # for each flight event, find out how much the bird was flying in total that day
  duration_date <- aggregate(flight_list$duration,by=list(as.Date(flight_list$start)),FUN=sum)
  names(duration_date) <- c("date", "total_daily_duration")

  flight_list <- merge(flight_list, duration_date, by.x="date", by.y="date")


  #--------------------------------------------------
  # find out how many seperate  flights were done that date
  flight_freq <- aggregate(flight_list$duration,by=list(as.Date(flight_list$start)),FUN=length)
  names(flight_freq) <- c("date", "total_daily_flight_number")

  flight_list <- merge(flight_list, flight_freq, by="date")


  #--------------------------------------------------
  # for that flight, how much did the pressure change

  flight_list$pressure_change <- 1:length(flight_list$total_daily_flight_number)
  flight_list$pressure_change <- unlist(lapply(1:length(flight_list$total_daily_flight_number), function(x) sum(abs(diff(pressure$obs[start[nrow(flight_list[1:x,])]:end[nrow(flight_list[1:x,])]]))))
  )

  #--------------------------------------------------
  # for each flight event, find out how much the pressure changed in total that day
  duration_date <- aggregate(flight_list$pressure_change, by=list(as.Date(flight_list$start)),FUN=sum)
  names(duration_date) <- c("date", "total_daily_P_change")

  flight_list <- merge(flight_list, duration_date, by.x="date", by.y="date")



  #--------------------------------------------------
  # for that flight, what was pressure like when the bird departed and when in stopped
  flight_list$P_dep_arr <- unlist(lapply(1:length(flight_list$total_daily_flight_number),
                                        function(x) abs(pressure$obs[start[nrow(flight_list[1:x,])]]- pressure$obs[end[nrow(flight_list[1:x,])]] )
  )
  )

  #--------------------------------------------------
  # for that flight, what was the total pressure range

  flight_list$pressure_range <- unlist(lapply(1:length(flight_list$total_daily_flight_number),
                                             function(x) max(pressure$obs[start[
                                               nrow(flight_list[1:x,])]:end[nrow(flight_list[1:x,])]])- min(
                                                 pressure$obs[start[nrow(flight_list[1:x,])]:end[nrow(flight_list[1:x,])]])
  ))

  #--------------------------------------------------
  # for that flight, what was pressure like the night before, and what was it like the next night

  if ("light" %in% availavariable){

    nights <- twl[twl$type==2,]

    fun <- function(x){
      to_add <- pressure[which(pressure$date > nights$tFirst[x] & pressure$date < nights$tSecond[x]),]
      to_add$night <- x
      to_add$night_before <- as.Date(nights$tFirst[x])
      to_add$night_after <- as.Date(nights$tSecond[x])
      return(to_add)
    }

    nightP <- do.call(rbind,lapply(1:length(nights$tFirst),FUN = fun))

    dt <- data.table(nightP)
    dt <- dt[,list(mean=mean(obs),sd=sd(obs)),by=nightP$night_before]
    dt <- dt %>% distinct
    dt <- as.data.frame(dt)

    colnames(dt) <- c("date", "mean_night_P", "sd_night_P")
    flight_list <- merge(flight_list, dt, by="date")

    colnames(dt) <- c("date", "mean_nextnight_P", "sd_nextnight_P")
    dt <-dt[-1,]
    dt$date <- dt$date-1
    flight_list <- merge(flight_list, dt, by="date", all.x=T)

    flight_list$night_P_diff <- abs(flight_list$mean_night_P - flight_list$mean_nextnight_P)

  }


  #--------------------------------------------------
  # for that flight, what was the proportion of time spent resting?
  if ("acceleration" %in% availavariable){


    flight_list$sum_activity <- 1:length(flight_list$total_daily_flight_number)
    flight_list$sum_activity <- unlist(lapply(1:length(flight_list$total_daily_flight_number),
                                             function(x) sum(activity$act[which(activity$date == pressure$date[start[nrow(flight_list[1:x,])]] ):which(activity$date == pressure$date[end[nrow(flight_list[1:x,])]] ) ])))

    flight_list$prop_resting <- unlist(lapply(1:length(flight_list$total_daily_flight_number),
                                             function(x) length(which(activity$act[which(activity$date == pressure$date[start[nrow(flight_list[1:x,])]] ):
                                                                                     which(activity$date == pressure$date[end[nrow(flight_list[1:x,])]] ) ] == 0 )) /
                                               length(activity$act[which(activity$date == pressure$date[start[nrow(flight_list[1:x,])]] ):
                                                                     which(activity$date == pressure$date[end[nrow(flight_list[1:x,])]] ) ])

    ))
    flight_list$prop_active <- 1-flight_list$prop_resting
  }

  #--------------------------------------------------
  # for that flight, what was the proportion of time spent resting?

  if (("light" %in% availavariable) & ("acceleration" %in% availavariable)) {

    nights <- twl[twl$type==2,]

    fun <- function(x){
      to_add <- activity[which(activity$date > nights$tFirst[x] & activity$date < nights$tSecond[x]),]
      to_add$night <- x
      to_add$night_before <- as.Date(nights$tFirst[x])
      to_add$night_after <- as.Date(nights$tSecond[x])
      return(to_add)

    }

    nightA <- do.call(rbind,lapply(1:length(nights$tFirst),FUN = fun))

    dt <- data.table(nightA)
    dt <- dt[,list(mean=mean(act),sd=sd(act), sum=sum(act)),by=nightA$night_before]
    dt <- dt %>% distinct
    dt <- as.data.frame(dt)

    colnames(dt) <- c("date", "mean_night_act", "sd_night_act", "sum_night_act")
    flight_list <- merge(flight_list, dt, by="date")

    colnames(dt) <- c("date", "mean_nextnight_act", "sd_nextnight_act", "sum_nextnight_act")
    dt <- dt[-1,]
    dt$date <- dt$date-1
    flight_list <- merge(flight_list, dt, by="date", all.x=T)

    flight_list$night_act_diff <- abs(flight_list$mean_night_act - flight_list$mean_nextnight_act)

  }

  return(flight_list)

}

