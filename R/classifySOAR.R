#' Make timetable
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param toPLOT can be true or false. If true then threshold is plotted according to plotTHLD()
#' @param method for the time being only supports manual. In this case the threshold = 2 as a default. This is the 15 minute hectopascal change
#' @param period number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 1x15min = 15 minutes of intense activity is considered a migratory flight
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
#' behaviour = classifySOAR(dta = PAM_data$pressure, period = 2, toPLOT = F)
#'
#' #plot the rclassification as an interactive plot
#' backup_options <- options()
#' options(viewer=NULL) # ensure it is viewed in internet browser
#' dygraphClassified(dta = PAM_data, timetable = behaviour$timetable)
#' options(backup_options) # restore previous viewer settings
#'
#' col=col=c("brown","cyan4","black","gold")
#' plot(PAM_data$pressure$date[2700:7000], PAM_data$pressure$obs[2700:7000],
#' col=col[behaviour$classification][2700:7000], type="o", pch=20, xlab="Date", ylab="Pressure")
#'
#' behaviour$timetable
#'
#' @export
classifySOAR <- function(dta , toPLOT = T, method = "PressureChange", threshold = 2, period = 2){
  if(method == "LowActivityPeriod"){
    # #for testing the function
    # dta=PAM_data$acceleration
    # method = "LowActivityPeriod"
    # period=36

      km = stats::kmeans(dta$act,centers=2)
      dta$clust = km$cluster

      # # now we take activity, partition it into magration or not based on duration of consecutive activity
      high_movement = as.numeric(which(table(dta$clust) == min(table(dta$clust))))#-1
      low_movement = as.numeric(which(table(dta$clust) == max(table(dta$clust))))

      type = "soaring"


      threshold = sum(min(max(dta$act[dta$clust==1]), max(dta$act[dta$clust==2])),
                      max(min(dta$act[dta$clust==1]), min(dta$act[dta$clust==2])))/2

      if(toPLOT == T) plotTHLD(dta$act, classification = km$cluster,threshold = threshold, type = "flapping")

      dta$movement = 1
      dta$movement[dta$act == 0] = 0
      # plot(dta$act[1:500],col=dta$movement[1:500]+3,type="o",pch=16)

      # get rid of 1-off missclassifications
      x = c(1,1,0,1,1)
      idx <- which(dta$movement == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$movement[i:(i+(length(x)-1))] == x))]
      dta$movement[idx+2] <- 1
      # idx <- which(dta$movement == x[1])

      # plot(dta$act[1:500],col=dta$movement[1:500]+3,type="o",pch=16)
      # x = c(1,1,0,1,1)
      # idx <- which(dta$movement == x[1])
      #
      # idx <- idx[sapply(idx, function(i) all(dta$movement[i:(i+(length(x)-1))] == x))]
      # dta$movement[idx+2] <- 1
      # idx <- which(dta$movement == x[1])


      Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
      colnames(Duration_table) = c("start","end","Duration (h)")
      Duration_table$start = as.POSIXct(Duration_table$start,tz=tz,format="%Y-%m-%d")
      Duration_table$end = as.POSIXct(Duration_table$end,tz=tz,format="%Y-%m-%d")
      Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

      # Count the length of each category
      # start=0
      # end=0

      x = c(0,1,1)
      start = which(dta$movement == x[1])
      start = start[sapply(start, function(i) all(dta$movement[i:(i+(length(x)-1))] == x))]
      start = start + 1
      x = c(1,1,0)
      end = which(dta$movement == x[1])
      end = end[sapply(end, function(i) all(dta$movement[i:(i+(length(x)-1))] == x))]
      end = end + 1

      if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
      if (length(end)>length(start)) start= start[1:length(end)]
      if (length(end)<length(start)) end= end[1:length(start)]

      # # make sure only periods where birds is flying longer than the flapping duration are stored
      index = which((end-start) >= 2)
      start = start[index]
      end = end[index]

      index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
      dta$clust[index] = 3
      dta$clust[dta$act == 0] = 4
      # plot(dta$act[1:500],col=dta$clust[1:500]+3,type="o",pch=16)

      # get rid of 1-off missclassifications of migration
      x = c(3,3,4,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3

      x = c(3,3,1,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3

      # plot(dta$act[1:500],col=dta$clust[1:500]+3,type="o",pch=16)
      x = c(3,3,2,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3

      # get rid of 1-off missclassifications of migration
      x = c(3,3,4,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3

      x = c(3,3,1,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3

      # plot(dta$act[1:500],col=dta$clust[1:500]+3,type="o",pch=16)
      x = c(3,3,2,3,3)
      idx <- which(dta$clust == x[1])
      idx <- idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
      dta$clust[idx+2] <- 3


      dta$mig=0
      dta$mig[dta$clust == 3]=1

      x = c(0,1,1)
      start = which(dta$mig == x[1])
      start = start[sapply(start, function(i) all(dta$mig[i:(i+(length(x)-1))] == x))]
      start = start + 1
      x = c(1,1,0)
      end = which(dta$mig == x[1])
      end = end[sapply(end, function(i) all(dta$mig[i:(i+(length(x)-1))] == x))]
      end = end + 1



      if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
      if (length(end)>length(start)) start= start[1:length(end)]
      if (length(end)<length(start)) end= end[1:length(start)]

      # # make sure only periods where birds is flying longer than the flapping duration are stored
      index = which((end-start) >= period)
      start = start[index]
      end = end[index]


      index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))
      dta$mig=0
      dta$mig[index]=1

      dta$clust = km$cluster
      dta$clust[index] = 3
      dta$clust[dta$act == 0] = 4
      # plot(dta$act[1:500],col=dta$clust[1:500]+3,type="o",pch=16)


      #look for start and end of migration
      # end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
      # start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)+ 1] ))
      #
      dur = difftime(dta$date[end], dta$date[start], tz= tz, units = "hours")
      info = data.frame(dta$date[start], dta$date[end], dur)
      names(info) = c("start","end","Duration (h)")
      Duration_table = rbind(Duration_table, info)

      Duration_table = Duration_table[-c(1,2),]
      # Duration_table
  }

  if(method == "PressureChange"){

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
    high_movement = 2
    low_movement = 1
  #
  #   x = c(low_movement,high_movement)
  #   start = which(dta$clust == x[1])
  #   start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   start = start[!is.na(start)]
  #   start = start + 1
  #
  #   x = c(high_movement, low_movement)
  #   end = which(dta$clust == x[1])
  #   end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   end = end[!is.na(end)]
  #   end = end + 1 # to make sure it indexes the last value as the end
  #
  #   if(end[1] < start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  #   if (length(end)>length(start)) start = start[1:length(end)]
  #   if (length(end)<length(start)) end = end[1:length(start)]
  #
  #   # make sure only periods where birds is flying longer than the flapping duration are stored
  #   index = which((end-start) >= period)
  #   start = start[index]
  #   end = end[index]#+1
  #
  #   index = unlist(sapply(1:length(start), function(i) start[i]:(end[i])))
  #   dta$clust[index] = 3
  #
  #   # plot(dta$date[2700:3000],dta$obs[2700:3000], col=dta$clust[2700:3000], type="o", pch=16)
  #
  #   # get rid of 1-off missclassifications
  #   x = c(3,high_movement,3)
  #   idx = which(dta$clust == x[1])
  #   idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   dta$clust[idx+1] = 3
  #
  #   x = c(3,low_movement,3)
  #   idx = which(dta$clust == x[1])
  #   idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   dta$clust[idx+1] = 3
  #
  #   dta$clust[abs(diff(dta$obs)) == 0] = 4
  #
  #   x = c(3,4)
  #   idx = which(dta$clust == x[1])
  #   idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   dta$clust[idx+1] = 3
  #
  #   x = c(3,4,3)
  #   idx = which(dta$clust == x[1])
  #   idx = idx[sapply(idx, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  #   dta$clust[idx+1] = 3
  #
  #
  # #
  # #   x = c(1,3,3)
  # #   start1 = which(dta$clust == x[1])
  # #   start1 = start1[sapply(start1, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   x = c(2,3,3)
  # #   start2 = which(dta$clust == x[1])
  # #   start2 = start2[sapply(start2, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   x = c(4,3,3)
  # #   start4 = which(dta$clust == x[1])
  # #   start4 = start4[sapply(start4, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   start = c(start1,start2,start4)[order(c(start1,start2,start4))]
  # #   start = start + 1
  # #
  # #
  # #   x = c(3,3,1)
  # #   start1 = which(dta$clust == x[1])
  # #   start1 = start1[sapply(start1, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   x = c(3,3,2)
  # #   start2 = which(dta$clust == x[1])
  # #   start2 = start2[sapply(start2, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   x = c(3,3,4)
  # #   start4 = which(dta$clust == x[1])
  # #   start4 = start4[sapply(start4, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
  # #   end = c(start1,start2,start4)[order(c(start1,start2,start4))]
  # #   end = end + 1
  # #
  # #
  # #   if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
  # #   if (length(end)>length(start)) start = start[1:length(end)]
  # #   if (length(end)<length(start)) end = end[1:length(start)]
  # #
  # #   # make sure only periods where birds is flying longer than the flapping duration are stored
  # #   index = which((end-start) >= period)
  # #   start = start[index]
  # #   end = end[index]
  #
  #   # #look for start and end of migration
  #   end = c(which(dta$clust ==3)[diff(which(dta$clust ==3)) > 1], which(dta$clust ==3)[length(which(dta$clust ==3))])
  #   start = c(which(dta$clust ==3)[1], (which(dta$clust ==3)[which(diff(which(dta$clust ==3)) > 1)] ))
  #
  #
  #   dur = difftime(dta$date[end], dta$date[start], tz= "UTC", units = "hours")
  #   info = data.frame(dta$date[start], dta$date[end], dur)
  #   names(info) = c("start","end","Duration (h)")
  #   Duration_table = rbind(Duration_table, info)
  #
  #   Duration_table = Duration_table[-c(1,2),]
  #
  #   # plot(abs(diff(dta$obs[2000:3000])), col= dta$clust[2000:3000])
  #   # plot(dta$obs[2500:3000], col= dta$clust[2500:3000])
  #
  #   return(list(type = type,
  #               timetable = Duration_table,
  #               classification = dta$clust,
  #               low_movement = low_movement,
  #               high_movement = high_movement,
  #               migration = 3,
  #               no_change = 4,
  #               threshold = threshold))
  #
  #

    x = c(low_movement,high_movement)
    start = which(dta$clust == x[1])
    start = start[sapply(start, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
    # abline(v=dta$date[start], col="red")
    x = c(high_movement, low_movement)
    end = which(dta$clust == x[1])
    end = end[sapply(end, function(i) all(dta$clust[i:(i+(length(x)-1))] == x))]
    # abline(v=dta$date[end], col="blue")

    if(end[1]< start[1]) end = end[-1] #if the series starts with an end not a start, remove the first ending
    if (length(end)>length(start)) start = start[1:length(end)]
    if (length(end)<length(start)) end = end[1:length(start)]

    # make sure only periods where birds is flying longer than the flapping duration are stored
    index = which((end-start) >= period)
    start = start[index]
    end = end[index]

    index = unlist(sapply(1:length(start), function(i) start[i]:end[i]))+1 # because of delay
    dta$clust[index] = 3

    # get rid of 1-off missclassifications
    x = c(3,low_movement,3)
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
    # dta$clust[dta$act == 0] = 4

    dta$clust[abs(diff(dta$obs)) == 0] = 4
  }

  return(list(type = type,
              timetable = Duration_table,
              classification = dta$clust,
              low_movement = low_movement,
              high_movement = high_movement,
              migration = 3,
              no_change = 4,
              threshold = threshold))

}

