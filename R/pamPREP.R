#' Derive classification data for soaring birds
#'
#' @param PAM_data PAM data to be used in the analysis
#' @param availavariable Variables to be used to derive metrics for classification. must have "pressure", but ideally `availavariable = c("pressure", "light", "acceleration")` if any of these are incomplete, do not use them
#' @param Pdiff_thld Pressure threshold. Only used when method="pressure".  This if pressure changes more than e.g. 2hpa over 30 minutes, then the bird is flying
#' @param light_thld Light threshold. Only used when method="darkness". This is the the light threshold for finding darkness, should be the same as for GeoLight::twilightCalc
#' @param method The type of event that is being classified. Can be "flap", "endurance", "rest", "pressure","light" or "darkness".If method = "pressure" then  it find periods where pressure has changed more than a certain threshold. If method = "flap", then the algorithm looks for sustained periods of high activity. If method = "endurance" it looks for sustained activity (low or high). If method = "rest+ then it looks for sustained periods of no activity. If method = "light" if looks for periods of sustained sunlight. If method = "darkness" if looks for periods of darkness
#' @param twl twilight estimates formatted according to twilightCalc in GeoLight package
#' @param tz Timeuzone. default is "UTC"
#' @param interp whether or not to interpolate the magnetic data. If FALSE, then NAs are left in the dataset
#'
#' @return a dataframe of derives metrhics based on pressure: `date`,`start` of event event,`end`,`duration` of event event,`total_daily_duration` spent flying that day,`total_daily_event_number` total number of event events that day,`pressure_change` absolute total pressure differences during that event,`P_dep_arr` difference in pressure between the start of the event and the end of the event,`pressure_range` difference between min and max pressure during that event,`mean_night_P` difference in mean pressure from the night before and the night after the event event,`sd_night_P`,`mean_nextnight_P`,`sd_nextnight_P`,`night_P_diff`,`sum_activity` total amount of activity during that event event,`prop_resting` proportion of time spent resting during that event,`prop_active` proportion of time spent active during that event event
#'
#' @examples
#' data(hoopoe)
#' PAM_data=hoopoe
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#'                              LightThreshold = 2, ask = FALSE)
#'
#' TOclassify = pamPREP(PAM_data = PAM_data,
#'                       method= "flap",
#'                       twl = twl)
#'
#'
#' @importFrom stats aggregate sd kmeans median
#' @importFrom data.table data.table
#' @importFrom GeoLight twilightCalc
#' @importFrom dplyr "%>%" distinct last
#' @importFrom zoo na.approx
#'
#' @export
pamPREP <- function(PAM_data,
                    availavariable = c("pressure", "light", "acceleration", "magnetic", "temperature"),
                    Pdiff_thld = 2,
                    light_thld = 1,
                    method = "pressure",
                    twl,
                    interp = TRUE,
                    tz="UTC"){

  if("pressure" %in% availavariable){
    pressure <- PAM_data$pressure
  }

  if("light" %in% availavariable){
    light <- PAM_data$light
  }

  if("acceleration" %in% availavariable){
    activity <- PAM_data$acceleration
  }
  if("magnetic" %in% availavariable){
    magnetic <- PAM_data$magnetic
    if(interp == TRUE){
      if (method == "pressure"){
        magnetic = merge(pressure, magnetic, all = TRUE)
      }
      if (method %in% c("light","darkness")){
        magnetic = merge(light$date, magnetic, all = TRUE)
      }
      if (method %in% c("rest", "flap", "endurance")){
        magnetic = merge(activity, magnetic, all = TRUE)
      }
      magnetic[,2:ncol(magnetic)] = na.approx(magnetic[,2:ncol(magnetic)])
    }
  }
  if("temperature" %in% availavariable){
    temperature <- PAM_data$temperature
  }



  if( method == "pressure"){
    if(!("pressure" %in% availavariable)){
      stop('There is no pressure data to classify pressure differences')
    }

    # find when a bird is flying
    pressure$Pdiff_thld <- c(0,abs(diff(pressure$obs)))
    pressure$flying <- 0
    pressure$flying[pressure$Pdiff_thld >= Pdiff_thld] <- 1

    # find start of event event
    x <- c(0,1)
    start <- which(pressure$flying == x[1])
    start <- start[sapply(start, function(i) all(pressure$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(pressure$flying == x[1])
    end <- end[sapply(end, function(i) all(pressure$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }
    #housekeeping
    if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]

    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = pressure$date[start],
                             end = pressure$date[end],
                             duration = difftime(pressure$date[end], pressure$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)



  }

  if(method == "flap"){
    if(!("acceleration" %in% availavariable)){
      stop('There is no activity data to classify flapping behaviour')
    }
    km = kmeans(activity$act,centers=2)

    threshold = sum(min(max(activity$act[km$clust==1],na.rm=TRUE), max(activity$act[km$clust==2],na.rm= TRUE)),
                    max(min(activity$act[km$clust==1],na.rm= TRUE), min(activity$act[km$clust==2],na.rm= TRUE)))/2


    activity$flying = ifelse(activity$act >= threshold, 1, 0)

    # find start of event event
    x <- c(0,1)
    start <- which(activity$flying == x[1])
    start <- start[sapply(start, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(activity$flying == x[1])
    end <- end[sapply(end, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }
    #housekeeping
    if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]


    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = activity$date[start],
                             end = activity$date[end],
                             duration = difftime(activity$date[end], activity$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)

  }

  if(method == "endurance"){
    if(!("acceleration" %in% availavariable)){
      stop('There is no activity data to classify sustained activity (low and high combined)')
    }

    activity$flying = ifelse(activity$act > 0, 1, 0)

    # find start of event event
    x <- c(0,1)
    start <- which(activity$flying == x[1])
    start <- start[sapply(start, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(activity$flying == x[1])
    end <- end[sapply(end, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }
    #housekeeping
    if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]


    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = activity$date[start],
                             end = activity$date[end],
                             duration = difftime(activity$date[end], activity$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)

  }

  if(method == "rest"){
    if(!("acceleration" %in% availavariable)){
      stop('There is no activity data to classify resting periods')
    }

    activity$flying = ifelse(activity$act == 0, 1, 0)

    # find start of event event
    x <- c(0,1)
    start <- which(activity$flying == x[1])
    start <- start[sapply(start, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(activity$flying == x[1])
    end <- end[sapply(end, function(i) all(activity$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }

    #housekeeping
    if (end[1] < start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]


    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = activity$date[start],
                             end = activity$date[end],
                             duration = difftime(activity$date[end], activity$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)

  }

  if(method == "darkness"){
    if(!("light" %in% availavariable)){
      stop('There is no light data to classify darkness periods')
    }

    light$flying = ifelse(light$obs < light_thld, 1, 0)

    # find start of event event
    x <- c(0,1)
    start <- which(light$flying == x[1])
    start <- start[sapply(start, function(i) all(light$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(light$flying == x[1])
    end <- end[sapply(end, function(i) all(light$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }

    #housekeeping
    if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]


    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = light$date[start],
                             end = light$date[end],
                             duration = difftime(light$date[end], light$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)

  }

  if(method == "light"){
    if(!("light" %in% availavariable)){
      stop('There is no light data to classify periods of continuous sunlight')
    }

    light$flying = ifelse(light$obs == max(light$obs), 1, 0)

    # find start of event event
    x <- c(0,1)
    start <- which(light$flying == x[1])
    start <- start[sapply(start, function(i) all(light$flying[i:(i+(length(x)-1))] == x))]
    start <- start

    # find end of event event
    x <- c(1, 0)
    end <- which(light$flying == x[1])
    end <- end[sapply(end, function(i) all(light$flying[i:(i+(length(x)-1))] == x))]

    #remove any back to back start end start end and merge them
    to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
    if(length(to_remove) >0){
      end <- end[-to_remove]
      start <- start[-(to_remove+1)]
    }
    #housekeeping
    if(end[1]< start[1]) end <- end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)<length(start)) start <- start[1:length(end)]
    if (length(end)>length(start)) end <- end[1:length(start)]

    # get rif of any 30 minute events
    index <- which((end-start) > 1)
    start <- start[index]
    end <- end[index]


    #--------------------------------------------------
    #For each event event, figure out how long it lasted
    event_list <- data.frame(start = light$date[start],
                             end = light$date[end],
                             duration = difftime(light$date[end],
                                                 light$date[start], tz= tz,
                                                 units = "hours"))
    event_list$duration <- as.numeric(event_list$duration )
    event_list$date <- as.Date(event_list$start)

  }



  #--------------------------------------------------
  # for each event event, find out how much the bird was flying in total that day
  duration_date <- aggregate(event_list$duration,by=list(as.Date(event_list$start)),FUN=sum)
  names(duration_date) <- c("date", "total_daily_duration")

  event_list <- merge(event_list, duration_date, by.x="date", by.y="date")


  #--------------------------------------------------
  # find out how many seperate  events were done that date
  event_freq <- aggregate(event_list$duration,by=list(as.Date(event_list$start)),FUN=length)
  names(event_freq) <- c("date", "total_daily_event_number")

  event_list <- merge(event_list, event_freq, by="date")



  if ("pressure" %in% availavariable){
    #--------------------------------------------------
    # for that event, how much did the pressure change

    event_list$cum_pressure_change <- 1:length(event_list$total_daily_event_number)
    event_list$cum_pressure_change <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                                    function(x) {sum(abs(diff(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                                             (pressure$date <= event_list$end[x])])))})
    )


    #--------------------------------------------------
    # for that event, how much did altitude change

    event_list$cum_altitude_change <- 1:length(event_list$total_daily_event_number)
    event_list$cum_altitude_change <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                                    function(x){
                                                      sum(abs(diff(altitudeCALC(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                                               (pressure$date <= event_list$end[x])]
                                                      ))))
                                                    } )
    )
    #--------------------------------------------------
    # for that event, how much did the bird go upwards

    event_list$cum_altitude_up <- 1:length(event_list$total_daily_event_number)
    event_list$cum_altitude_up <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                                function(x) {
                                                  test = diff(altitudeCALC(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                                          (pressure$date <= event_list$end[x])]))
                                                  test = test[test>0]
                                                  test = sum(test)
                                                  return(test)
                                                }
    ))


    #--------------------------------------------------
    # for each event event, find out how much the pressure changed in total that day
    duration_date <- aggregate(event_list$cum_pressure_change, by=list(as.Date(event_list$start)),FUN=sum)
    names(duration_date) <- c("date", "total_daily_P_change")

    event_list <- merge(event_list, duration_date, by.x="date", by.y="date")



    #--------------------------------------------------
    # for that event, what was pressure like when the bird departed and when in stopped
    event_list$P_dep_arr <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x) {abs( pressure$obs[last(which(pressure$date <= event_list$end[x]))] -
                                                              pressure$obs[which(pressure$date >= event_list$start[x])[1]])}
                                          # pressure$obs[start[nrow(event_list[1:x,])]]-
                                          #                 pressure$obs[end[nrow(event_list[1:x,])]] )
    )
    )

    #--------------------------------------------------
    # for that event, what was the total pressure range

    event_list$pressure_range <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                               function(x) {max(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                               (pressure$date <= event_list$end[x])])-
                                                   min(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                      (pressure$date <= event_list$end[x])])}
    ))

    event_list$altitude_range <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                               function(x) {abs(altitudeCALC(max(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                                                (pressure$date <= event_list$end[x])]))-
                                                                  altitudeCALC(min(pressure$obs[(pressure$date >= event_list$start[x]) &
                                                                                                  (pressure$date <= event_list$end[x])])))}
    ))
  }
  #--------------------------------------------------
  # for that event, what was pressure like the night before, and what was it like the next night

  if (("light" %in% availavariable) & ("pressure" %in% availavariable)){

    nights <- twl[twl$type==2,]
    fun <- function(x){
      to_add <- pressure[which(pressure$date > nights$tFirst[x] & pressure$date < nights$tSecond[x]),]#old
      to_add$night <- x
      to_add$night_before <- as.Date(nights$tFirst[x])
      to_add$night_after <- as.Date(nights$tSecond[x])
      return(to_add)
    }

    nightP <- do.call(rbind,lapply(1:(length(nights$tFirst)-1),FUN = fun))
    obs = NULL
    dt <- data.table(nightP)
    dt <- dt[,list(mean=mean(obs),sd=sd(obs)),by=nightP$night_before]
    dt <- dt %>% distinct()
    dt <- as.data.frame(dt)

    colnames(dt) <- c("date", "mean_night_P", "sd_night_P")
    event_list <- merge(event_list, dt, by="date")

    colnames(dt) <- c("date", "mean_nextnight_P", "sd_nextnight_P")
    dt <-dt[-1,]
    dt$date <- dt$date-1
    event_list <- merge(event_list, dt, by="date", all.x = TRUE)

    event_list$night_P_diff <- abs(event_list$mean_night_P - event_list$mean_nextnight_P)

  }


  #--------------------------------------------------
  # for that event, what was the proportion of time spent resting?
  if ("acceleration" %in% availavariable){

    event_list$median_activity <- 1:length(event_list$total_daily_event_number)
    event_list$median_activity <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                             function(x) {median(activity$act[(activity$date >= event_list$start[x]) &
                                                                             (activity$date <= event_list$end[x])], na.rm=TRUE)}))

    event_list$sum_activity <- 1:length(event_list$total_daily_event_number)
    event_list$sum_activity <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                             function(x) {sum(activity$act[(activity$date >= event_list$start[x]) &
                                                                             (activity$date <= event_list$end[x])])}))

    event_list$prop_resting <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                             function(x) {length(which(activity$act[(activity$date >= event_list$start[x]) &
                                                                                      (activity$date <= event_list$end[x])] == 0 )) /
                                                 length(activity$act[(activity$date >= event_list$start[x]) &
                                                                       (activity$date <= event_list$end[x]) ])}))
    event_list$prop_active <- 1-event_list$prop_resting
  }

  #--------------------------------------------------
  # for that event, what was the proportion of time spent resting the night before and the night after?

  if (("light" %in% availavariable) & ("acceleration" %in% availavariable)) {

    nights <- twl[twl$type==2,]

    fun <- function(x){
      to_add <- activity[which(activity$date > nights$tFirst[x] & activity$date < nights$tSecond[x]),]
      to_add$night <- x
      to_add$night_before <- as.Date(nights$tFirst[x])
      to_add$night_after <- as.Date(nights$tSecond[x])
      return(to_add)

    }

    nightA <- do.call(rbind,lapply(1:(length(nights$tFirst)-1),FUN = fun))
    act = NULL

    dt <- data.table(nightA)
    dt <- dt[,list(mean=mean(act),sd=sd(act), sum=sum(act)),by=nightA$night_before]
    dt <- dt %>% distinct()
    dt <- as.data.frame(dt)

    colnames(dt) <- c("date", "mean_night_act", "sd_night_act", "sum_night_act")
    event_list <- merge(event_list, dt, by="date")

    colnames(dt) <- c("date", "mean_nextnight_act", "sd_nextnight_act", "sum_nextnight_act")
    dt <- dt[-1,]
    dt$date <- dt$date-1
    event_list <- merge(event_list, dt, by="date", all.x=T)

    event_list$night_act_diff <- abs(event_list$mean_night_act - event_list$mean_nextnight_act)

  }

  #--------------------------------------------------
  # for that event, what was the median body position, and how variable was it
  if ("acceleration" %in% availavariable){
    event_list$median_pitch <- 1:length(event_list$total_daily_event_number)
    event_list$median_pitch <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                             function(x){
                                               median(activity$pit[(activity$date >= event_list$start[x]) &
                                                                     (activity$date <= event_list$end[x])], na.rm=TRUE)
                                             }))
    event_list$sd_pitch <- 1:length(event_list$total_daily_event_number)
    event_list$sd_pitch <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                         function(x){
                                           sd(activity$pit[(activity$date >= event_list$start[x]) &
                                                             (activity$date <= event_list$end[x])], na.rm=TRUE)
                                         }))
  }

  #--------------------------------------------------
  # for that event, how light or dark was it
  if ("light" %in% availavariable){
    event_list$median_light <- 1:length(event_list$total_daily_event_number)
    event_list$median_light <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                             function(x){
                                               median(light$obs[(light$date >= event_list$start[x]) &
                                                                  (light$date <= event_list$end[x])], na.rm=TRUE)
                                             }))


    nights <- twl[twl$type==2,]

    if (method %in% "pressure"){
      nightime <- rep_len(0, length.out = length(pressure$date))
      start2 = unlist(lapply(1:nrow(nights),
                             function(x) which(pressure$date >= nights$tFirst[x])[1]))
      end2 = unlist(lapply(1:nrow(nights),
                           function(x) last(which(pressure$date <= nights$tSecond[x]))))
    }

    if (method %in% c("rest", "endurance","flap")){
      nightime <- rep_len(0, length.out = length(activity$date))
      start2 = unlist(lapply(1:nrow(nights),
                             function(x) which(activity$date >= nights$tFirst[x])[1]))
      end2 = unlist(lapply(1:nrow(nights),
                           function(x) last(which(activity$date <= nights$tSecond[x]))))
    }
    if (method %in% c("light","darkness")){
      nightime <- rep_len(0, length.out = length(light$date))
      start2 = unlist(lapply(1:nrow(nights),
                             function(x) which(light$date >= nights$tFirst[x])[1]))
      end2 = unlist(lapply(1:nrow(nights),
                           function(x) last(which(light$date <= nights$tSecond[x]))))
    }
    # housekeeping
    start2 = start2[complete.cases(start2)]
    end2 = end2[complete.cases(end2)]

    if(end2[1]< start2[1]) end2 <- end2[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end2)<length(start2)) start2 <- start2[1:length(end2)]
    if (length(end2)>length(start2)) end2 <- end2[1:length(start2)]
    index2 = unlist(lapply(1:length(end2),
                           function(x) start2[x]:end2[x]))

    nightime[index2] = 1
    if (method %in% "pressure"){
      event_list$nightime <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                           function(x){
                                             median(nightime[(pressure$date >= event_list$start[x]) &
                                                               (pressure$date <= event_list$end[x])], na.rm=TRUE)
                                           }))
    }

    if (method %in% c("rest", "endurance","flap")){
      event_list$nightime <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                           function(x){
                                             median(nightime[(activity$date >= event_list$start[x]) &
                                                               (activity$date <= event_list$end[x])], na.rm=TRUE)
                                           }))
    }
    if (method %in% c("light","darkness")){
      event_list$nightime <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                           function(x){
                                             median(nightime[(light$date >= event_list$start[x]) &
                                                               (light$date <= event_list$end[x])], na.rm=TRUE)
                                           }))
    }


  }


  #--------------------------------------------------
  # for that event, what was the median tri-axial information
  if ("magnetic" %in% availavariable){
    event_list$median_gX <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$gX[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
    event_list$median_gY <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$gY[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
    event_list$median_gZ <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$gZ[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
    event_list$median_mX <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$mX[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
    event_list$median_mY <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$mY[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
    event_list$median_mZ <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                          function(x){
                                            median(magnetic$mZ[(magnetic$date >= event_list$start[x]) &
                                                                 (magnetic$date <= event_list$end[x])], na.rm=TRUE)
                                          }))
  }
  #--------------------------------------------------
  # for that event, what was the median temperature and the change in temp
  if ("temperature" %in% availavariable){
    event_list$median_temp <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                            function(x){
                                              median(temperature$obs[(temperature$date >= event_list$start[x]) &
                                                                       (temperature$date <= event_list$end[x])], na.rm=TRUE)
                                            }))
    event_list$sd_temp <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                            function(x){
                                              sd(temperature$obs[(temperature$date >= event_list$start[x]) &
                                                                       (temperature$date <= event_list$end[x])], na.rm=TRUE)
                                            }))
    event_list$cum_temp_change <- unlist(lapply(1:length(event_list$total_daily_event_number),
                                            function(x){
                                              sum(abs(diff(temperature$obs[(temperature$date >= event_list$start[x]) &
                                                                       (temperature$date <= event_list$end[x])], na.rm=TRUE)))
                                            }))


  }


  return(event_list)

}

