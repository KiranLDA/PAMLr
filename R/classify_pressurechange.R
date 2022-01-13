#' Classify pressure change
#'
#' @description This function uses a change in pressure greater than a certain threshold to classify diving or flying
#'
#' @param dta data stored as a list see str(hoopoe) for example format
#' @param thld threshold that is used di discinguish between background pressure fluctuations and flights or dives, by default it is 2hPa
#' @param duration duration in hours of the pressure displacement used to quantify a long duration dive or flight, default is 1h
#' @param tz timezone, default is "UTC"
#'
#' @return timetable: a timetable for when the species was migrating or not,
#' @return classification: a classification timeseries where datetime corresponds to activity, and
#' @return no_pressurechange: the value in classification which corresponds to background pressure changes from weather
#' @return short_pressurechange: the value in classification which corresponds to a short change in pressure
#' @return long_pressurechange: the value in classification which corresponds to a long change in pressure
#' @return threshold: the threshold between low and high activity
#'
#' @references Bäckman, J., Andersson, A., Alerstam, T., Pedersen, L., Sjöberg, S., Thorup, K. and Tøttrup, A.P., 2017. Activity and migratory flights of individual free‐flying songbirds throughout the annual cycle: method and first case study. Journal of avian biology, 48(2), pp.309-319.
#' @references Liechti, F., Bauer, S., Dhanjal-Adams, K.L., Emmenegger, T., Zehtindjiev, P. and Hahn, S., 2018. Miniaturized multi-sensor loggers provide new insight into year-round flight behaviour of small trans-Sahara avian migrants. Movement ecology, 6(1), p.19.
#' @references Bruderer, B., Peter, D., Boldt, A. and Liechti, F., 2010. Wing‐beat characteristics of birds recorded with tracking radar and cine camera. Ibis, 152(2), pp.272-291.
#'
#' @examples
#' #specify the data location
#' data(hoopoe)
#' # make sure the cropping period is in the correct date format
#' start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
#'
#' # Crop the data
#' PAM_data= create_crop(hoopoe,start,end)
#'
#' behaviour = classify_pressurechange(dta = PAM_data$pressure)
#'
#' col=c("black","cyan4","gold")
#' plot(PAM_data$pressure$date[2000:2800],PAM_data$pressure$obs[2000:2800],
#'      col=col[behaviour$classification+1][2000:2800],
#'      type="o", pch=20,
#'      xlab="Date",
#'      ylab="Pressure")
#'
#' behaviour$timetable
#'
#' @importFrom data.table data.table
#' @export
classify_pressurechange <- function(dta ,
                                    thld = 2,
                                    duration = 1,
                                    tz= "UTC"){

  # find when a bird is flying
  dta$Pdiff <- c(0,abs(diff(dta$obs)))
  dta$flying <- 0
  dta$flying[dta$Pdiff >= thld] <- 1

  # find start of event event
  x <- c(0,1)
  start <- which(dta$flying == x[1])
  start <- start[sapply(start, function(i) all(dta$flying[i:(i+(length(x)-1))] == x))]
  start <- start

  # find end of event event
  x <- c(1, 0)
  end <- which(dta$flying == x[1])
  end <- end[sapply(end, function(i) all(dta$flying[i:(i+(length(x)-1))] == x))]

  #remove any back to back start end start end and merge them
  to_remove <- which(start[2:length(start)] - end[1:(length(start)-1)] == 1)
  if(length(to_remove) >0){
    end <- end[-to_remove]
    start <- start[-(to_remove+1)]
  }

  #if the series starts with an end not a start, remove the first ending
  if(end[1]< start[1]) end <- end[-1]
  if (length(end)<length(start)) start <- start[1:length(end)]
  if (length(end)>length(start)) end <- end[1:length(start)]

  # get rif of any 30 minute events that are 1 off
  index <- which((end-start) > 1)
  start <- start[index]
  end <- end[index]

  #--------------------------------------------------
  #For each event, figure out how long it lasted
  event_list <- data.frame(start = dta$date[start],
                           end = dta$date[end],
                           dur = difftime(dta$date[end], dta$date[start], tz= tz,
                                          units = "hours"))
  event_list$dur <- as.numeric(event_list$dur )


  dta$clust = 0
  for ( i in 1:nrow(event_list)){
    dta$clust[dta$date <= event_list$end[i] & dta$date >= event_list$start[i] ] = 1
  }

  #remove short events
  event_list = event_list[event_list$dur >= duration,]
  for ( i in 1:nrow(event_list)){
    dta$clust[dta$date <= event_list$end[i] & dta$date >= event_list$start[i] ] = 2
  }

  Duration_table = data.frame(event_list)
  names(Duration_table)[3] = "Duration (h)"






  return(list(timetable = Duration_table,
              classification = dta$clust,
              no_pressurechange = 0,
              short_pressurechange = 1,
              long_pressurechange = 2,
              threshold = thld,
              duration = duration))
}
