#' Make timetable
#'
#' @param dta the input light data to be classified e.g. bee_eater$light
#' @param period number of timepoints after which behaviour is considered soaring e.g. for light recorded every 5 minutesand period = 3, then 3x5min = 15 minutes of continuous light is considered soaring flight
#' @param toPLOT can be true or false. If true then threshold is plotted according to plotTHLD()
#' @param method for the time being only supports "kmeans" and "manual", if methods= "kmeans" then duration_threshold is overwritten
#' @param duration_threshold the duration in hours where flight is not longer soaring, but migratory
#' @param act_threshold This is the threshold between no activity and activity, be default it is set to 1
#' @param keep_one_off_missclassifications Can be TRUE or FALSE, if true, any one-off soaring missclassifications are replaced with soaring
#' @param tz timezone, default is "UTC"
#'
#'
#'
#' @return a timetable for when the species was migrating or not
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
#' dta = bee_eater$acceleration
#'
#' behaviour = sustainedACT(dta , period = 3,
#'                          toPLOT = TRUE,
#'                          act_threshold = 1,
#'                          keep_one_off_missclassifications = FALSE,
#'                          method = "kmeans", tz= "UTC")
#'
#' pressure_states = merge(bee_eater$pressure,
#'                         data.frame(bee_eater$acceleration,states=behaviour$classification ),
#'                         all=FALSE)
#' par(mfrow=c(2,1), mar=c(4,4,0.5,0.5))
#' col=c("black","brown","cyan4","gold")
#' plot(pressure_states$date, pressure_states$obs, type="o",
#'      bg= col[pressure_states$states+1], pch=21,
#'      xlab="Date", ylab="Pressure",
#'      cex = ifelse(pressure_states$states==behaviour$migrating,1,0.7))
#'
#' #migration is well classified, but soaring is over estimated
#' behaviour = sustainedACT(dta , period = 3, toPLOT = FALSE,
#'                          act_threshold = 1,
#'                          duration_threshold=1.5,
#'                          keep_one_off_missclassifications = TRUE,
#'                          method = "manual", tz= "UTC")
#'
#' pressure_states = merge(bee_eater$pressure,
#'                         data.frame(bee_eater$acceleration,states=behaviour$classification ),
#'                         all=FALSE)
#'
#' col=c("black","brown","cyan4","gold")
#' plot(pressure_states$date, pressure_states$obs, type="o",
#'      bg= col[pressure_states$states+1], pch=21,
#'      xlab="Date", ylab="Pressure",
#'      cex = ifelse(pressure_states$states==behaviour$migrating,1,0.7))
#'
#' @importFrom stats kmeans
#'
#' @export
sustainedACT <- function(dta , period = 3, toPLOT = TRUE,
                          act_threshold = 1,
                          duration_threshold = 2,
                          keep_one_off_missclassifications = FALSE,
                          method = c("manual","kmeans"), tz= "UTC"){


  # specify migration values
  migrating = 3
  sustained_act = 2
  act = 1
  no_act = 0

  # now we take high activity, partition it into magration or not based on duration
  dta$clust = no_act
  dta$clust[dta$act >= act_threshold] = act

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
    x = c(act,no_act,act)
    idx = which(dta$clust == x[1])
    idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
    dta$clust[idx+1] = act
  }

  # find start and end of continuous activity
  x = c(no_act,act,act)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  x = c(act, act,no_act)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]

  # make sure start and end are the same length
  if(end[1] < start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end) < length(start)) start = start[1:length(end)]
  if (length(end) >length(start)) end = end[1:length(start)]

  # make sure only periods where birds is displaying sustained activity for longer than the "period" are kept
  index = which((end-start) >= period)
  start = start[index]+1
  end = end[index]

  # store and label these
  index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
  dta$clust[index] = sustained_act

  # store information on nighttime
  # dta$clust[dta$obs <= act_threshold] = no_act

  # look for start and end of migration
  end = c(which(dta$clust ==sustained_act)[diff(which(dta$clust ==sustained_act)) > 1], which(dta$clust ==sustained_act)[length(which(dta$clust ==sustained_act))])
  start = c(which(dta$clust ==sustained_act)[1], (which(dta$clust ==sustained_act)[which(diff(which(dta$clust ==sustained_act)) > 1)+ 1] ))

  # Estimate duration of sustained_act instances
  dur = as.numeric(difftime(dta$date[end], dta$date[start], tz= tz, units = "hours"))



  if (method == "kmeans"){
    # method = "kmeans"
    km = kmeans(dur,centers=2)
    classification = km$cluster
    DURthreshold = sum(min(max(dur[classification==1],na.rm = TRUE), max(dur[classification==2],na.rm = TRUE)),
                       max(min(dur[classification==1],na.rm = TRUE), min(dur[classification==2],na.rm = TRUE)))/2
  }
  if (method == "manual"){
    # method = "manual"
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

  type = "sustained activity"
  if(toPLOT == TRUE) plotTHLD(dur, classification = classification, threshold = DURthreshold, type = "light")

  # Only class as migratory the  continuous act periods which last longer than the threshold
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
              no_act = no_act,
              act = act,
              sustained_act = sustained_act,
              migrating = migrating,
              threshold = DURthreshold))
}

