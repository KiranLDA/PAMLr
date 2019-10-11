#' Classify flapping flight
#'
#' @description This function uses activity data to classify migratory flapping flight.
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param period number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 3x5min = 15 minutes of intense activity is considered flight
#' @param to_plot can be true or false. If true then threshold is plotted according to plot_histogram()
#' @param method for the time being only supports "kmeans", but will later also include maybe
#' @param tz timezone, default is "UTC"
#'
#' @return timetable: a timetable for when the species was migrating or not,
#' @return classification: a classification timeseries where datetime corresponds to activity, and
#' @return no_movement: the value in classification which corresponds to no movement
#' @return low_movement: the value in classification which corresponds to low activity
#' @return high_movement: the value in classification which corresponds to high activity
#' @return migration: the value in classification which corresponds to migratory flapping flight
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
#' PAM_data= wrangle_crop(hoopoe,start,end)
#' str(PAM_data)
#'
#' behaviour = classify_flap(dta = PAM_data$acceleration, period = 12)
#'
#' col=c("brown","cyan4","black","gold")
#' plot(PAM_data$acceleration$date[6000:8000],PAM_data$acceleration$act[6000:8000],
#'      col=col[behaviour$classification][6000:8000],
#'      type="o", pch=20,
#'      xlab="Date",
#'      ylab="Activity")
#'
#' behaviour$timetable
#'
#' @importFrom stats kmeans poisson
#' @importFrom depmixS4 depmix fit posterior
#'
#' @export
classify_flap <- function(dta ,
                         period = 12,
                         to_plot = TRUE,
                         method = "kmeans",
                         tz= "UTC"){
  if (method == "kmeans"){
    km = kmeans(dta$act,centers=2)
    dta$clust = km$cluster
  }

  if (method == "hmm"){
    # poisson() gaussian()multinomial("identity")
    hmm <- depmix(act ~ 1, family = poisson() , nstates = 2, data=dta[dta$act>0,])
    hmmfit <- fit(hmm, verbose = FALSE)
    dta$clust = NA
    dta$clust[dta$act>0] <- posterior(hmmfit)$state
  }

  type = "flapping"
  threshold = sum(min(max(dta$act[dta$clust==1],na.rm=TRUE), max(dta$act[dta$clust==2],na.rm= TRUE)),
                  max(min(dta$act[dta$clust==1],na.rm= TRUE), min(dta$act[dta$clust==2],na.rm= TRUE)))/2

  if(to_plot == TRUE) plot_histogram(dta$act, classification = dta$clust, threshold = threshold, type = type)

  # Count the length of each category
  start=0
  end=0

  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz=tz,format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz=tz,format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_movement = as.numeric(which(table(dta$clust) == min(table(dta$clust),na.rm= TRUE)))
  low_movement = as.numeric(which(table(dta$clust) == max(table(dta$clust),na.rm= TRUE)))
  dta$clust[is.na(dta$clust)] =  low_movement

  # get rid of 1-off missclassifications
  x = c(high_movement,low_movement,high_movement)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = high_movement

  x = c(low_movement,high_movement)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  x = c(high_movement, low_movement)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]

  if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end)<length(start)) start= start[1:length(end)]
  if (length(end)>length(start)) end= end[1:length(start)]


  # make sure only periods where birds is flying longer than the flapping duration are stored
  index = which((end-start) >= period)
  start = start[index]
  end = end[index]

  index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
  dta$clust[index] = 3


  #look for start and end of migration
  end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
  start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)+ 1] ))

  dur = difftime(dta$date[end], dta$date[start], tz= tz, units = "hours")
  info = data.frame(dta$date[start], dta$date[end], dur)
  names(info) = c("start","end","Duration (h)")
  Duration_table = rbind(Duration_table, info)


  # order so that low movement is lower than high movement
  if (high_movement == 1){
    dta$clust[dta$clust == 1] = 999
    dta$clust[dta$clust == 2] = 1
    dta$clust[dta$clust == 999] = 2
  }

  Duration_table = Duration_table[-c(1,2),]
  dta$clust[dta$act == 0] = 0
  return(list(timetable = Duration_table,
              classification = dta$clust,
              no_movement = 0,
              low_movement = 1,
              high_movement = 2,
              migration = 3,
              threshold = threshold))
}

