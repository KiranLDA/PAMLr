#' Calculate twilight events (sunrise/sunset) from light intensity measurements
#' over time
#'
#' This is a simplified version (no plotting) of the old Function from GeoLight to define twilight
#' events (sunrise/sunset) at times when the light intensity
#' measurements (\emph{light}) pass the defined light intensity threshold. An
#' interactive plot can be drawn to assess the calculations and improve e.g.
#' select only the realistic events.
#'
#' The R Package TwGeos has far better ways to calculate twilight times from ligth recordings.
#' See https://geolocationmanual.vogelwarte.ch/ for details.
#'
#' @param datetime date and time of light intensity measurements e.g.
#' 2008-12-01 08:30 "UTC" (see:
#' \code{\link{as.POSIXct}},\link[=Sys.timezone]{time zones}).
#' @param light \code{numerical} value of the light intensity (usually
#' arbitrary units).
#' @param preSelection codelogical, if TRUE a pre selection of all calculated
#' twiligth events will be offered within the interactive process (ask=TRUE).
#' @param LightThreshold the light intensity threshold for the twilight event
#' calibration. If \code{Default}, it will be set slightly above (3 units) the
#' baseline level (measurement during the night).
#' @param maxLight if the geolocator record the maximum light value of a
#' certain time span, give the interval of maximum recordings in minutes (e.g.
#' 5).
#' @param allTwilights \code{logical}, if TRUE the function returns a list with
#' two tables
#' @return if allTwilights=FALSE, a \code{data frame}. Each row contains two
#' subsequent twilight events (\emph{tFirst, tSecond}) and \emph{type} defining
#' wether \emph{tFirst} refers to sunrise (1) or sunset (2). If
#' allTwilights=TRUE, a \code{list} with the data frame described in the
#' previous sentence and a data frame with all light intensities and a column
#' describing whether each row refers to sunrise (1), sunset (2) or to none of
#' these categories (0).
#' @note Depending on shading during light intensity measurements (e.g. due to
#' vegetation, weather, etc., see Lisovski et \emph{al.} 2012) the light
#' intensities may pass the light intensity threshold several times during the
#' day, resulting false sunrises and sunsets. It is highly recommended to check
#' the derived events visually (\code{ask=TRUE}).Twilight events can be deleted
#' and undeleted by clicking the (first) mouse button at the particular
#' position in the graph. The second mouse buttom (or esc) moves the time
#' series forward. Note, that a backward option is not included.
#' @author Simeon Lisovski
#' @export twilightCalc
twilightCalc <- function(datetime, light, LightThreshold=TRUE, preSelection=TRUE, maxLight=NULL, allTwilights=FALSE)
{
  if(class(datetime)[1]!="POSIXct") {
    stop(sprintf("datetime need to be provided as POSIXct class object."), call. = F)

  } else {
    bas <- data.frame(datetime=as.POSIXct(as.character(datetime),"UTC"),light)
  }


  if (is.numeric(LightThreshold))
  {
    LightThreshold <- as.numeric(LightThreshold)
    min <- min(bas$light)
  } else {
    # Basic level
    r <- as.data.frame(table(bas$light))
    nr <- as.numeric(which.max(r$Freq[as.numeric(r[,1])<mean(bas$light)]))
    LightThreshold <- (as.numeric(as.character(r[nr,1])))+3
  }

  out <- i.preSelection(bas$datetime,bas$light, LightThreshold)[,-1]

  if(!preSelection) out$mod <- 0

  # if(ask)
  # {
  #   n   <- nrow(bas)
  #   nn  <- n%/%nsee + 1
  #   cutsub <- cut(1:n, nn)
  #   picks <- NULL
  #
  #   for(i in 1:nn){
  #     sub <- cutsub == levels(cutsub)[i]
  #
  #     repeat{
  #       plot(bas[sub,1],bas[sub,2],type="o",cex=0.6,pch=20,ylab="Light intensity",xaxs="i",xaxt="n",xlab="",
  #            main=paste(as.Date(min(bas[sub,1]))," to ", as.Date(max(bas[sub,1]))," (end: ",as.Date(max(bas[,1])),")",sep=""))
  #       abline(h=LightThreshold,col="blue",lty=2)
  #       abline(v=out[out$mod==0,1],col="orange",lty=ifelse(out[out$mod==0,2]==1,1,2))
  #       points(out[,1],rep(LightThreshold,nrow(out[,])),col=ifelse(out$mod==0,"orange","grey"),pch=20,cex=0.8)
  #
  #       axis(1,at=out[seq(from=1,to=nrow(out),length.out=(nrow(out)%/%2)),1],
  #            labels=substring(as.character(out[seq(from=1,to=nrow(out),length.out=(nrow(out)%/%2)),1]),6,16),cex=0.7)
  #
  #       legend("topright",lty=c(3,1,2),lwd=c(1.3,2,2),col=c("blue",rep("orange",2)),c("Light\nThreshold","sunrise","sunset"),cex=1,bg="white")
  #
  #       nr <- identify(out[,1],rep(LightThreshold,nrow(out)),n=1,plot=F)
  #       ifelse(length(nr)>0,ifelse(out$mod[nr]==0,out$mod[nr]<-1,out$mod[nr]<-0),break)
  #     }
  #   }
  #
  #   cat("Thank you!\n\n")
  #   graphics.off()
  # }

  results <- list()


  out <- subset(out,out$mod==0)[,-3]

  raw <- data.frame(datetime=c(as.POSIXct(datetime,"UTC"),as.POSIXct(out$datetime,"UTC")),
                    light=c(light,rep(LightThreshold,nrow(out))),type=c(rep(0,length(datetime)),out$type))

  raw <- raw[order(raw$type),]
  raw <- raw[-which(duplicated(as.character(raw$datetime),fromLast=T)),]
  raw <- raw[order(raw$datetime),]

  results$allTwilights <- raw

  opt <- data.frame(tFirst=as.POSIXct("1900-01-01 01:01","UTC"),tSecond=as.POSIXct("1900-01-01 01:01","UTC"),type=0)
  row <- 1
  for (k in 1:(nrow(out)-1))
  {
    if (as.numeric(difftime(out[k,1],out[k+1,1]))< 24 & out[k,1] != out[k+1,1])
    {
      opt[row,1] <- out[k,1]
      opt[row,2] <- out[k+1,1]
      opt[row,3] <- out$type[k]

      row <- row+1
    }
  }

  if(is.numeric(maxLight))
  {
    opt$tFirst[opt$type==2] <- opt$tFirst[opt$type==2] - (maxLight*60)
    opt$tSecond[opt$type==1] <- opt$tSecond[opt$type==1] - (maxLight*60)
  }

  if(allTwilights) {
    results$consecTwilights <- opt
    return(results)
  } else {
    return (opt)}
}


i.preSelection <- function(date, light, LightThreshold){

  dt <- cut(datetime,"1 hour")
  st <- as.POSIXct(levels(dt),"UTC")

  raw <- data.frame(datetime=dt,light=light)

  h  <- tapply(light,dt,max)
  df1 <- data.frame(datetime=st+(30*60),light=as.numeric(h))

  smooth <- i.twilightEvents(df1[,1], df1[,2], LightThreshold)
  smooth <- data.frame(id=1:nrow(smooth),smooth)
  raw    <- i.twilightEvents(datetime, light, LightThreshold)
  raw <- data.frame(id=1:nrow(raw),raw)

  ind2 <- rep(NA,nrow(smooth))
  for(i in 1:nrow(smooth)){
    tmp <- subset(raw,datetime>=(smooth[i,2]-(90*60)) & datetime<=(smooth[i,2]+(90*60)))

    if(smooth[i,3]==1) ind3 <- tmp$id[which.min(tmp[,2])]
    if(smooth[i,3]==2) ind3 <- tmp$id[which.max(tmp[,2])]
    ind2[i] <- ind3
  }


i.twilightEvents <- function (datetime, light, LightThreshold)
  {
    df <- data.frame(datetime, light)
    ind1 <- which((df$light[-nrow(df)] < LightThreshold & df$light[-1] >
                     LightThreshold) | (df$light[-nrow(df)] > LightThreshold &
                                          df$light[-1] < LightThreshold) | df$light[-nrow(df)] ==
                    LightThreshold)
    bas1 <- cbind(df[ind1, ], df[ind1 + 1, ])
    bas1 <- bas1[bas1[, 2] != bas1[, 4], ]
    x1 <- as.numeric(unclass(bas1[, 1]))
    x2 <- as.numeric(unclass(bas1[, 3]))
    y1 <- bas1[, 2]
    y2 <- bas1[, 4]
    m <- (y2 - y1)/(x2 - x1)
    b <- y2 - (m * x2)
    xnew <- (LightThreshold - b)/m
    type <- ifelse(bas1[, 2] < bas1[, 4], 1, 2)
    res <- data.frame(datetime = as.POSIXct(xnew, origin = "1970-01-01",
                                            tz = "UTC"), type)
    return(res)
  }



  res <- data.frame(raw,mod=1)
  res$mod[ind2] <- 0

  return(res)

}
