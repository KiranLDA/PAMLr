#' Make timetable
#'
#' @param dta data stored as a list see str(data(PAM_data)) for example format
#' @param flapping_duration number of timepoints after which behaviour is considered migratory e.g. for hoopoes, 3x5min = 15 minutes of intense activity is considered a migratory flight
#'
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
#' behaviour = classifyFLAP(dta = PAM_data$acceleration, flapping_duration = 3)
#'
#'
#' col=col=c("brown","cyan4","black","gold")
#' plot(PAM_data$acceleration$date[2000:4000],PAM_data$acceleration$act[2000:4000],
#' col=col[behaviour$classification][2000:4000], type="o", pch=20, xlab="Date", ylab="Activity")
#'
#' behaviour$timetable
#'
#' @export
classifyFLAP <- function(dta , flapping_duration = 3){
  km = kmeans(dta$act,centers=2)
  dta$clust = km$cluster

  par(mar=c(4,4,1,1))
  hist(dta$act[dta$act != 0],  breaks = (max(dta$act[dta$act != 0])-min(dta$act[dta$act != 0])), xlab="Activity",
       main ="Initial High / Low activity Classification", border=F)
  plot(hist(dta$act[dta$clust==1 & dta$act != 0],
            breaks = (max(dta$act[dta$clust==1 & dta$act != 0])-min(dta$act[dta$clust==1 & dta$act != 0])), plot=F),
       col=rgb(1,0,0,1/4), add=T, border=F)
  plot(hist(dta$act[dta$clust==2 & dta$act != 0],
            breaks = (max(dta$act[dta$clust==2 & dta$act != 0])-min(dta$act[dta$clust==2 & dta$act != 0])), plot=F),
       col=rgb(0,0,0,1/4), add=T, border=F)
  # abline(v=min(max(dta$act[dta$clust==1 & dta$act != 0]), max(dta$act[dta$clust==2 & dta$act != 0])), lty=2)
  # abline(v=max(min(dta$act[dta$clust==1 & dta$act != 0]), min(dta$act[dta$clust==2 & dta$act != 0])), lty=2)
  threshold = sum(min(max(dta$act[dta$clust==1]), max(dta$act[dta$clust==2])),
                  max(min(dta$act[dta$clust==1]), min(dta$act[dta$clust==2])))/2
  abline(v=threshold, lty=2)
  text(x = threshold,  (max((hist(dta$act[dta$act != 0],
                               breaks = (max(dta$act[dta$act != 0])-min(dta$act[dta$act != 0])),
                               plot=F)$counts)) - min((hist(dta$act[dta$act != 0],
                                                            breaks = (max(dta$act[dta$act != 0])-min(dta$act[dta$act != 0])),
                                                            plot=F)$counts)))/2,
       paste0("threshold = ",threshold), pos=4)



  # Count the length of each category
  start=0
  end=0

  Duration_table = data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) = c("start","end","Duration (h)")
  Duration_table$start = as.POSIXct(Duration_table$start,tz="UTC",format="%Y-%m-%d")
  Duration_table$end = as.POSIXct(Duration_table$end,tz="UTC",format="%Y-%m-%d")
  Duration_table$`Duration (h)` = as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_activity = as.numeric(which(table(dta$clust) == min(table(dta$clust))))#-1
  low_activity = as.numeric(which(table(dta$clust) == max(table(dta$clust))))

  for (i in 2:nrow(dta)){
    if(dta$clust[i] == high_activity & dta$clust[i-1] != high_activity){
      start=i
    }
    if(dta$clust[i] != high_activity & dta$clust[i-1] == high_activity){
      end=(i-1)
    }
    if(end>start){
      duration = end - start
      if(duration > flapping_duration){
        dta$clust[start:end] = 3
        dur = difftime(dta$date[end], dta$date[start], tz= "UTC", units = "hours")
        # dur = (dta$date[end]-dta$date[start])/(60*60)
        info = data.frame(dta$date[start], dta$date[end], dur)
        names(info) = c("start","end","Duration (h)")
        Duration_table = rbind(Duration_table, info)
      }
    }
  }

  Duration_table= unique(Duration_table)
  Duration_table[,3] = as.numeric(Duration_table[,2] - Duration_table[,1])/(60*60)
  Duration_table = Duration_table[-1,]
  dta$clust[dta$act == 0] = 4
  return(list(timetable = Duration_table,
              classification = dta$clust,
              low_activity = low_activity,
              high_activity = high_activity,
              migration = 3,
              no_activity = 4,
              threshold = threashold))
}

