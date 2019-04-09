#' Derive classification data for soaring birds
#'
#' @param dta PAM data to be used in the analysis
#' @param interp_NA By default TRUE. Whether or not to interpolate NAs in dataset that the rollapply is used on
#' @param resolution_out Temporal resolution of output dataset. By defaukt 15 mins. Must be in minutes unless the units are changed
#' @param window Window over which to apply the rolling window. By defaukt 120 mins. Equivalent to zoo::rollapply(,width = window,) Must be in minutes unless the units are changed
#' @param units By default"mins", but also supports "hours" and "secs"
#'
#' @return a dataframe of derived metrics including the median, standard deviation, minimum, maximum, cumulative difference and range over the period
#'
#' @examples
#' data(swift)
#' PAM_data = swift
#'
#' # crop the data to get rid of no good periods
#' start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-04-21","%Y-%m-%d", tz="UTC")
#' PAM_data = cutPAM(PAM_data, start, end)
#'
#' TOclassify = rollPAM(dta = list(pressure = PAM_data$pressure,
#'                                 acceleration = PAM_data$acceleration),
#'                      resolution_out = 60 ,
#'                      window = 24*60)
#' head(TOclassify)
#' plot(TOclassify$cumu_diff_pressure, type="l")
#'
#'
#' @importFrom zoo na.approx
#' @importFrom stats complete.cases median sd
#' @importFrom dplyr "%>%"
#' @importFrom zoo rollapply
#'
#'
#' @export
rollPAM  <- function(dta,
                     interp_NA = TRUE,
                     resolution_out = 15 ,#( in minutes)
                     window = 120, # mist be in minutes unless the units are changed
                     units="mins" # supports "hours" and "secs"
                     ){

  PAM_data = dta

  # # For testing the function
  # data("swift")
  # PAM_data = swift
  # start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
  # end = as.POSIXct("2017-04-21","%Y-%m-%d", tz="UTC")
  # PAM_data = cutPAM(PAM_data, start, end)

  if (units == "hours"){
    window = window * 60
    resolution_out = resolution_out * 60
  }
  if(units == "secs"){
    window = window / 60
    resolution_out = resolution_out / 60
  }

  #specify the window
  period = window/resolution_out

  if("id" %in% names(PAM_data)){
    to_remove = which(names(PAM_data) == "id")
    PAM_data[[to_remove]] <- NULL
  }
  if("obs" %in% colnames(PAM_data$light)){
    colnames(PAM_data$light)[which(colnames(PAM_data$light) == "obs")] = "light"
  }
  if("obs" %in% colnames(PAM_data$pressure)){
    colnames(PAM_data$pressure)[which(colnames(PAM_data$pressure) == "obs")] = "pressure"
  }
  if("obs" %in% colnames(PAM_data$temperature)){
    colnames(PAM_data$temperature)[which(colnames(PAM_data$temperature) == "obs")] = "temperature"
  }

  #----------------------------------------------
  # Merge the dataset to the specified resolution

  new = Reduce(function(...) base::merge(..., all = TRUE), PAM_data)
  dates_out = data.frame(date=seq.POSIXt(new$date[1],
                                         new$date[length(new$date)],
                                         by=resolution_out*60))
  index = which(new$date %in% dates_out$date)
  to_change = 2:ncol(new)

  test = do.call(cbind, lapply(to_change,
                               FUN = function(col){
                                 if(any(is.na(new[,col])) & interp_NA == TRUE){
                                     first = which(!is.na(new[,col]))[1]
                                     last = which(!is.na(new[,col]))[length(which(!is.na(new[,col])))]
                                     new[first:last,col] <- zoo::na.approx(x = new$date[first:last],
                                                                           object = new[first:last,col],
                                                                           xout = new$date)
                                  }
                                 return(new[,col])
                               }))



  test=data.frame(new$date,test)
  colnames(test) = colnames(new)
  test = test[index,]

  #----------------------------------------------
  # Median

  median = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, median, na.rm=TRUE )
  }))
  median = data.frame(test$date[round(period/2): (dim(median)[1] + round(period/2)-1)],
                      median)
  colsnam = paste0("median_",colnames(new)[to_change])
  colnames(median) <- c("date", colsnam)


  #----------------------------------------------
  # Standard Deviation

  sd = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, sd, na.rm=TRUE )
  }))
  sd = data.frame(test$date[round(period/2): (dim(sd)[1] + round(period/2)-1)],
                      sd)
  colsnam = paste0("sd_",colnames(new)[to_change])
  colnames(sd) <- c("date", colsnam)

  #----------------------------------------------
  # Cumulative difference

  cumu_diff = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, function(x) sum(abs(diff(x)), na.rm=TRUE ))
  }))
  cumu_diff = data.frame(test$date[round(period/2): (dim(cumu_diff)[1] + round(period/2)-1)],
                         cumu_diff)
  colsnam = paste0("cumu_diff_",colnames(new)[to_change])
  colnames(cumu_diff) <- c("date", colsnam)

  #----------------------------------------------
  # Maximum

  max = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, max, na.rm=TRUE )
  }))
  max = data.frame(test$date[round(period/2): (dim(max)[1] + round(period/2)-1)],
                   max)
  colsnam = paste0("max_",colnames(new)[to_change])
  colnames(max) <- c("date", colsnam)

  #----------------------------------------------
  # Minimum

  min = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, min, na.rm=TRUE )
  }))
  min = data.frame(test$date[round(period/2): (dim(min)[1] + round(period/2)-1)],
                   min)
  colsnam = paste0("min_",colnames(new)[to_change])
  colnames(min) <- c("date", colsnam)

  #----------------------------------------------
  # Sum

  sum = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, sum, na.rm=TRUE )
  }))
  sum = data.frame(test$date[round(period/2): (dim( sum)[1] + round(period/2)-1)],
                   sum)
  colsnam = paste0("sum_",colnames(new)[to_change])
  colnames( sum) <- c("date", colsnam)


  #----------------------------------------------
  # Range

  range = do.call(cbind,lapply( to_change, function(i){
    zoo::rollapply(test[,i],period, function(x) max(x, na.rm=TRUE) - min(x,na.rm=TRUE) )
  }))
  range = data.frame(test$date[round(period/2): (dim(range)[1] + round(period/2)-1)],
                     range)
  colsnam = paste0("range_",colnames(new)[to_change])
  colnames(range) <- c("date", colsnam)



  out = cbind(test[round(period/2): (dim(range)[1] + round(period/2)-1),],
              median[,to_change],
              sd[,to_change],
              sum[,to_change],
              min[,to_change],
              max[,to_change],
              cumu_diff[,to_change],
              range[,to_change])
}
