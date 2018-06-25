#' Make timetable
#'
#' @param PAM_data data stored as a list see str(data(PAM_data)) for example format
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
#' plot(PAM_data$acceleration$date, PAM_data$acceleration$act)
#'
#' # at first glance it looks like the logger was removed off a birds and left in a
#  # rucksack- remove un-needed data
#' PAM_data$acceleration = PAM_data$acceleration[(PAM_data$acceleration$date >= "2016-07-30" & PAM_data$acceleration$date <= "2017-06-01"),]
#'
#' behaviour <- classify_flapping
#' col=c("brown","cyan4","black","gold", "grey","pink")
#' plot(dta$date[2000:4000],dta$act[2000:4000],col=col[dta$clust][2000:4000], type="o", pch=20)
#'
#'
#'
#'
#' @export
classify_flapping <- function(dta = PAM_data$acceleration, flapping_duration = 3){
  km = kmeans(dta$act,centers=2)
  dta$clust = km$cluster

  # Count the length of each category
  start=0
  end=0

  Duration_table <- data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
  colnames(Duration_table) <- c("start","end","Duration (h)")
  Duration_table$start <- as.POSIXct(Duration_table$start,tz="UTC",format="%Y-%m-%d")
  Duration_table$end <- as.POSIXct(Duration_table$end,tz="UTC",format="%Y-%m-%d")
  Duration_table$`Duration (h)` <- as.numeric(Duration_table$`Duration (h)`)

  # now we take high activity, partition it into magration or not based on duration
  high_activity <- as.numeric(which(table(dta$clust)==min(table(dta$clust))))-1

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
        info = data.frame(dta$date[start], dta$date[end],
                           (dta$date[end]-dta$date[start])/(60*60))
        colnames(info) = c("start","end","Duration (h)")
        Duration_table = rbind(Duration_table, info)
      }
    }
  }

  Duration_table= unique(Duration_table)
  Duration_table[,3] = as.numeric(Duration_table[,2] - Duration_table[,1])/(60*60)
  Duration_table =Duration_table[-1,]
  dta$clust[dta$clust == 4]
  return(timtable = Duration_table, classification = dta$clust)
}



# differente into low and high activity based on density distribution /histogram of data
km <- kmeans(dta$obs,centers=2)
dta$clust <- km$cluster
col=c("brown","cyan4","black","gold", "grey","pink")
plot(dta$obs,col=col[dta$clust])

# now put all 0 as a category of their own
dta$clust[which(dta$obs == 0)] <-0
plot(dta$obs,col=col[dta$clust+1])

# Count the length of each category
start=0
end=0

Duration_table <- data.frame(matrix(c("2015-01-01","2015-01-01","2015-01-01","2015-01-01",0,0),nrow=2))
colnames(Duration_table) <- c("start","end","Duration (h)")
Duration_table$start <- as.POSIXct(Duration_table$start,tz="UTC",format="%Y-%m-%d")
Duration_table$end <- as.POSIXct(Duration_table$end,tz="UTC",format="%Y-%m-%d")
Duration_table$`Duration (h)` <- as.numeric(Duration_table$`Duration (h)`)

# now we take high activity, partition it into magration or not based on duration
high_activity <- as.numeric(which(table(dta$clust)==min(table(dta$clust))))-1

for (i in 2:nrow(dta)){
  if(dta$clust[i] == high_activity & dta$clust[i-1] != high_activity){
    start=i
  }
  if(dta$clust[i] != high_activity & dta$clust[i-1] == high_activity){
    end=(i-1)
  }
  if(end>start){
    duration = end - start
    if(duration > 3){
      dta$clust[start:end] <- 3
      info <- data.frame(dta$datetime[start], dta$datetime[end],
                         (dta$datetime[end]-dta$datetime[start])/(60*60))
      colnames(info) <- c("start","end","Duration (h)")
      Duration_table <- rbind(Duration_table, info)
    }
  }
}

Duration_table= unique(Duration_table)
Duration_table[,3] = as.numeric(Duration_table[,2] - Duration_table[,1])/(60*60)
Duration_table =Duration_table[-1,]
Duration_table


write.csv(Duration_table,
          paste0("O:\\PAM analysis\\R-code\\hoopoes\\PAM_birds\\timetables\\",bird_id[j],"_timetable.csv"),
          row.names = F)
