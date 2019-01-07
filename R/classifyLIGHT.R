#' Make timetable
#'
#' @param dta the input light data to be classified e.g. bee_eater$light
#' @param period number of timepoints after which behaviour is considered soaring e.g. for light recorded every 5 minutesand period = 3, then 3x5min = 15 minutes of continuous light is considered soaring flight
#' @param toPLOT can be true or false. If true then threshold is plotted according to plotTHLD()
#' @param method for the time being only supports "kmeans" and "manual", if methods= "kmeans" then duration_threshold is overwritten
#' @param duration_threshold the duration in hours where flight is not longer soaring, but migratory
#' @param light_threshold This is the threshold between nightime and daystime
#' @param keep_one_off_missclassifications Can be TRUE or FALSE, if true, any one-off soaring missclassifications are replaced with soaring
#' @param tz timezone, default is "UTC"
#'
#'
#'
#' @return a timetable for when the species was migrating or not
#'
#'
#'
#'
#' @examples
#' data("bee_eater")
#'
#' start = as.POSIXct("2015-07-12","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2016-06-01","%Y-%m-%d", tz="UTC")
#'
#' bee_eater = cutPAM(bee_eater, start, end)
#' quickPLOT(bee_eater, measurements = c("pressure", "light"))
#'
#' dta = bee_eater$light
#'
#' behaviour = classifyLIGHT(dta , period = 3, toPLOT = T,
#' light_threshold = 5, #duration_threshold = 2,
#' keep_one_off_missclassifications = TRUE,
#' method = "kmeans", tz= "UTC")
#'
#'
#' col=c("brown","cyan4","black","gold")
#' plot(dta$date,dta$obs, type="o", col= col[behaviour$classification], pch=16,
#' cex=ifelse(behaviour$classification == behaviour$migrating,1,0))
#'
#' behaviour$timetable
#'
#' @importFrom stats kmeans
#' @importFrom depmixS4 depmix fit posterior
#'
#' @export
classifyLIGHT <- function(dta , period = 3, toPLOT = T,
                          light_threshold = 5,
                          duration_threshold = 2,
                          keep_one_off_missclassifications = TRUE,
                          method = c("manual","kmeans"), tz= "UTC"){


  # now we take high activity, partition it into magration or not based on duration
  migrating = 4
  soaring = 3
  high_light = 2
  low_light = 1
  no_light = 0


  # start by specifying
  dta$clust = low_light
  dta$clust[dta$obs == max(dta$obs)] = high_light

  # Count the length of each category
  start=0
  end=0

  # set up table to store information and ensure dates are in the correct format
  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz=tz,format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz=tz,format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)


  # get rid of 1-off missclassifications
  if(keep_one_off_missclassifications == TRUE){
    x = c(high_light,low_light,high_light)
    idx = which(dta$clust == x[1])
    idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
    dta$clust[idx+1] = high_light
  }

  # find start and end of continuous sunlight
  x = c(low_light,high_light,high_light)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  x = c(high_light, high_light,low_light)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]

  # make sure start and end are the same length
  if(end[1] < start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end) < length(start)) start = start[1:length(end)]
  if (length(end) >length(start)) end = end[1:length(start)]

  # make sure only periods where birds is "soaring" longer than the "period" are kept
  index = which((end-start) >= period)
  start = start[index]+1
  end = end[index]

  # store these as soaring periods
  index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
  dta$clust[index] = soaring

  # store information on nighttime
  dta$clust[dta$obs <= light_threshold] = no_light

  #look for start and end of migration
  end = c(which(dta$clust ==soaring)[diff(which(dta$clust ==soaring)) > 1], which(dta$clust ==soaring)[length(which(dta$clust ==soaring))])
  start = c(which(dta$clust ==soaring)[1], (which(dta$clust ==soaring)[which(diff(which(dta$clust ==soaring)) > 1)+ 1] ))

  #estimate duration of soaring instances
  dur = as.numeric(difftime(dta$date[end], dta$date[start], tz= tz, units = "hours"))



  if (method == "kmeans"){
    method = "kmeans"
    km = kmeans(dur,centers=2)
    classification = km$cluster
    DURthreshold = sum(min(max(dur[classification==1],na.rm=T), max(dur[classification==2],na.rm=T)),
                       max(min(dur[classification==1],na.rm=T), min(dur[classification==2],na.rm=T)))/2
  }
  if (method == "manual"){
    method == "manual"
    DURthreshold = duration_threshold
    classification = ifelse(dur >= DURthreshold,1,2)
  }

  # if (method == "hmm"){
  #   # poisson() gaussian()multinomial("identity")
  #   hmm = depmix( dur ~ 2, family = gaussian() , nstates = 3,  ntimes=length(dur))
  #   hmmfit = fit(hmm, verbose = FALSE)
  #   classification = posterior(hmmfit)$state
  # }
  #

  type = "light"
  if(toPLOT == T) plotTHLD(dur, classification = classification, threshold = DURthreshold, type = type)

  # Only class as migratory the  continuous light periods which last longer than the threshold
  index=which(dur >= DURthreshold)
  dur = difftime(dta$date[end[index]], dta$date[start[index]], tz= tz, units = "hours")
  info = data.frame(dta$date[start[index]], dta$date[end[index]], dur)
  names(info) = c("start","end","Duration (h)")
  Duration_table = rbind(Duration_table, info)

  #get rid of the data used to initiate the table
  Duration_table = Duration_table[-c(1,2),]

  #identify these periods and add these classes to the data
  idx = unlist(lapply(1:length(index), FUN = function(x){start[index[x]]:end[index[x]]}))
  dta$clust[idx] = migrating

  # plot(dta$date[75000:80000],dta$obs[75000:80000], type="o", col=dta$clust[75000:80000], pch=16)
  # plot(dta$date,dta$obs, type="o", col=dta$clust+1, pch=16, cex=ifelse(dta$clust == migrating,1,0))

  return(list(type = type,
              timetable = Duration_table,
              classification = dta$clust,
              no_light = no_light,
              low_light = low_light,
              high_light = high_light,
              soaring = soaring,
              migrating = migrating,
              threshold = DURthreshold))
}

