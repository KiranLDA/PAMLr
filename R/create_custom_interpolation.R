#' Reduces the data to a specified temporal resolution
#'
#' @description This function takesthe typical PAM_data input, which is a nested list of different sensor data, all formatted at different time resolutions. By specifying a particular sensor, all data are formatted at the same temporal resolution as this data. By default, the median of data with smaller temporal resolution are kept, and data are interpolated.
#'
#'
#' @param dta raw pam data see `data(bee_eater` for example
#' @param varint the variable of interest.Sipprots c("pressure","light","acceleration","temperature","magnetic"). For instance if all other variables should be summarised at the same temporal resolution as this specified variable.
#' @param interp Default TRUE. Whether or not to replace NAs with interpolated values
#' @param summary Can be "sum", "median" or "none". What type of summary variable to give when condensing data -
#'
#' @return reduced/summarised and interpolated dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#'
#' start = as.POSIXct("2015-08-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2016-06-21","%Y-%m-%d", tz="UTC")
#' PAM_data = create_crop(PAM_data, start, end)
#'
#' reduced_dta = create_custom_interpolation(PAM_data , "pressure", interp = FALSE)
#' head(reduced_dta)
#'
#' reduced_dta = create_custom_interpolation(PAM_data , "act", interp = FALSE)
#' head(reduced_dta)
#'
#' @importFrom zoo na.approx
#' @importFrom stats complete.cases
#'
#' @export
create_custom_interpolation <- function(dta,
                                        varint,
                                        interp = TRUE,
                                        summary = "median"){

  PAM_data = dta

  if (varint == "acceleration") varint <- "act"
  if (varint == "magnetic") varint <- "gX"

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

  new = Reduce(function(...) merge(..., all = TRUE), PAM_data)
  index = which(!is.na(new[,which(colnames(new) %in% varint)]))
  to_change = which(!(colnames(new) %in% c(varint, "date")))

  test = do.call(cbind, lapply(to_change,
                        FUN = function(col){
                          if(any(is.na(new[,col]))){
                            if (interp == TRUE){
                              first = which(!is.na(new[,col]))[1]
                              last = which(!is.na(new[,col]))[length(which(!is.na(new[,col])))]
                              # new[first:last,col] <- zoo::na.approx(new[first:last,col])
                              new[first:last,col] <- zoo::na.approx(x = new$date[first:last],
                                                                    object = new[first:last,col],
                                                                    xout = new$date)
                            }
                            # }  else{
                            #    new <- new[index,col]
                            #  }
                          } else {
                            if(summary == "median"){
                            median=c(new[index[1],col],
                                     unlist(lapply(1:(length(index)-1), FUN = function(x){
                                       median( new[(index[x]+1): index[x+1], col], na.rm=TRUE)})))
                            new[index,col] <- median
                            }
                            if(summary == "sum"){
                              sum=c(new[index[1],col],
                                       unlist(lapply(1:(length(index)-1), FUN = function(x){
                                         sum( new[(index[x]+1): index[x+1], col], na.rm=TRUE)})))
                              new[index,col] <- sum
                            }
                          }
                          return(new[,col])
                        }))


  # colnames(test) = colnames(new)[to_change]
  test=data.frame(new$date,  new[,varint], test)
  colnames(test) = c("date", varint, colnames(new)[to_change])
  test = test[complete.cases(test[,varint]),]
  rownames(test) = c()

  return(test)
}
