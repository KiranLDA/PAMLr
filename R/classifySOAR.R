#' Make timetable
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param toPLOT can be true or false. If true then threshold is plotted according to plotTHLD()
#' @param method for the time being only supports manual. In this case the threshold = 2 as a default. This is the 15 minute hectopascal change
#' @param soaring_duration number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 1x15min = 15 minutes of intense activity is considered a migratory flight
#' @param threshold the manual threshold at which activity and non activity is drawn. in this case a change of greater than 2 hpa /15 minutes ios unlikely to have be caused by weather
#'
#' @return a timetable for when the species was migrating or not
#'
#' @examples
#' #specify the data location
#' data(PAM_data)
#' str(PAM_data)
#'
#' #plot the activity to see if it looks ok
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs, xlab = "Time", ylab = "Pressure (hPa)")
#'
#' # at first glance it looks like the logger was removed off a birds and left in arucksack
#  # so remove un-needed data
#' PAM_data$pressure = PAM_data$pressure[((PAM_data$pressure$date >= "2016-07-30")
#' & (PAM_data$pressure$date <= "2017-06-01")),]
#'
#' behaviour = classifySOAR(dta = PAM_data$pressure, soaring_duration = 2, toPLOT = F)
#'
#'
#' col=col=c("brown","cyan4","black","gold")
#' plot(PAM_data$pressure$date[2700:3000], PAM_data$pressure$obs[2700:3000],
#' col=col[behaviour$classification][2700:3000], type="o", pch=20, xlab="Date", ylab="Pressure")
#'
#' behaviour$timetable
#'
#' @export
classifySOAR <- function(dta , toPLOT = T, method = "manual", threshold = 2, soaring_duration = 2){

  dta$clust = c(ifelse(abs(diff(dta$obs))>threshold,2,1),1)
  type = "soarglide"

  if(toPLOT == T) plotTHLD(abs(diff(dta$obs)),  classification = dta$clust, threshold = threshold, type=type)

  # Count the length of each category
  start=0
  end=0

  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz="UTC",format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz="UTC",format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_change = 2
  low_change = 1

  x = c(low_change,high_change)
  start = which(dta$clust == x[1])
  start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  start = start[!is.na(start)]
  start = start + 1

  x = c(high_change, low_change)
  end = which(dta$clust == x[1])
  end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  end = end[!is.na(end)]
  end = end + 1 # to make sure it indexes the last value as the end

  if(end[1] < start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  if (length(end)>length(start)) start = start[1:length(end)]
  if (length(end)<length(start)) end = end[1:length(start)]

  # make sure only periods where birds is flying longer than the flapping duration are stored
  index = which((end-start) >= soaring_duration)
  start = start[index]
  end = end[index]#+1

  index = unlist(sapply(1:length(start), function(i) start[i]:(end[i])))
  dta$clust[index] = 3

  plot(dta$date[2700:3000],dta$obs[2700:3000], col=dta$clust[2700:3000], type="o", pch=16)

  # get rid of 1-off missclassifications
  x = c(3,high_change,3)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = 3

  x = c(3,low_change,3)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = 3

  dta$clust[abs(diff(dta$obs)) == 0] = 4

  x = c(3,4)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = 3

  x = c(3,4,3)
  idx = which(dta$clust == x[1])
  idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  dta$clust[idx+1] = 3


#
#   x = c(1,3,3)
#   start1 = which(dta$clust == x[1])
#   start1 = start1[sapply(start1, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   x = c(2,3,3)
#   start2 = which(dta$clust == x[1])
#   start2 = start2[sapply(start2, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   x = c(4,3,3)
#   start4 = which(dta$clust == x[1])
#   start4 = start4[sapply(start4, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   start = c(start1,start2,start4)[order(c(start1,start2,start4))]
#   start = start + 1
#
#
#   x = c(3,3,1)
#   start1 = which(dta$clust == x[1])
#   start1 = start1[sapply(start1, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   x = c(3,3,2)
#   start2 = which(dta$clust == x[1])
#   start2 = start2[sapply(start2, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   x = c(3,3,4)
#   start4 = which(dta$clust == x[1])
#   start4 = start4[sapply(start4, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
#   end = c(start1,start2,start4)[order(c(start1,start2,start4))]
#   end = end + 1
#
#
#   if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
#   if (length(end)>length(start)) start = start[1:length(end)]
#   if (length(end)<length(start)) end = end[1:length(start)]
#
#   # make sure only periods where birds is flying longer than the flapping duration are stored
#   index = which((end-start) >= soaring_duration)
#   start = start[index]
#   end = end[index]

  # #look for start and end of migration
  end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
  start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)] ))


  dur = difftime(dta$date[end], dta$date[start], tz= "UTC", units = "hours")
  info = data.frame(dta$date[start], dta$date[end], dur)
  names(info) = c("start","end","Duration (h)")
  Duration_table = rbind(Duration_table, info)

  Duration_table = Duration_table[-c(1,2),]

  # plot(abs(diff(dta$obs[2000:3000])), col= dta$clust[2000:3000])
  # plot(dta$obs[2500:3000], col= dta$clust[2500:3000])

  return(list(type = type,
              timetable = Duration_table,
              classification = dta$clust,
              low_change = low_change,
              high_change = high_change,
              migration = 3,
              no_change = 4,
              threshold = threshold))
}

