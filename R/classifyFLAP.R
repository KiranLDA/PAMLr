#' Make timetable
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param period number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 3x5min = 15 minutes of intense activity is considered flight
#' @param toPLOT can be true or false. If true then threshold is plotted according to plotTHLD()
#' @param method for the time being only supports "kmeans", but will later also include maybe
#' @param tz timezone, default is "UTC"
#' @return a timetable for when the species was migrating or not
#'
#' @examples
#' #specify the data location
#' data(PAM_data)
#' str(PAM_data)
#'
#' #plot the activity to see if it looks ok
#' plot(PAM_data$acceleration$date, PAM_data$acceleration$act, xlab="Time", ylab="activity")
#'
#' # at first glance it looks like the logger was removed off a birds and left in arucksack
#  # so remove un-needed data
#' PAM_data$acceleration = PAM_data$acceleration[((PAM_data$acceleration$date >= "2016-07-30")
#' & (PAM_data$acceleration$date <= "2017-06-01")),]
#'
#' behaviour = classifyFLAP(dta = PAM_data$acceleration, period = 4)
#'
#'
#' col=col=c("brown","cyan4","black","gold")
#' plot(PAM_data$acceleration$date[2000:4000],PAM_data$acceleration$act[2000:4000],
#' col=col[behaviour$classification][2000:4000], type="o", pch=20, xlab="Date", ylab="Activity")
#'
#' behaviour$timetable
#'
#' @export
classifyFLAP <- function(dta , period = 3, toPLOT = T, method = "kmeans", tz= "UTC"){
  if (method == "kmeans"){
    km = stats::kmeans(dta$act,centers=2)
    dta$clust = km$cluster
  }

  type = "flapping"
  threshold = sum(min(max(dta$act[dta$clust==1]), max(dta$act[dta$clust==2])),
                  max(min(dta$act[dta$clust==1]), min(dta$act[dta$clust==2])))/2

  if(toPLOT == T) plotTHLD(dta$act, classification = km$cluster,threshold = threshold, type = type)

  # Count the length of each category
  start=0
  end=0

  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz=tz,format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz=tz,format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_activity = as.numeric(which(table(dta$clust) == min(table(dta$clust))))#-1
  low_activity = as.numeric(which(table(dta$clust) == max(table(dta$clust))))

  x = c(low_activity,high_activity)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  x = c(high_activity, low_activity)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]

  if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end)>length(start)) start= start[1:length(end)]
  if (length(end)<length(start)) end= end[1:length(start)]

  # make sure only periods where birds is flying longer than the flapping duration are stored
  index = which((end-start) >= period)
  start = start[index]
  end = end[index]

  index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
  dta$clust[index] = 3

  # get rid of 1-off missclassifications
  x = c(3,low_activity,3)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = 3

  #look for start and end of migration
  end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
  start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)+ 1] ))

  dur = difftime(dta$date[end], dta$date[start], tz= tz, units = "hours")
  info = data.frame(dta$date[start], dta$date[end], dur)
  names(info) = c("start","end","Duration (h)")
  Duration_table = rbind(Duration_table, info)

  Duration_table = Duration_table[-c(1,2),]
  dta$clust[dta$act == 0] = 4
  return(list(type = type,
              timetable = Duration_table,
              classification = dta$clust,
              low_activity = low_activity,
              high_activity = high_activity,
              migration = 3,
              no_activity = 4,
              threshold = threshold))
}

