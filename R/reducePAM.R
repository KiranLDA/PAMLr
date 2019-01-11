#' Reduces the data to a specified temporal resolution
#'
#' @param dta raw pam data see `data(bee_eater` for example
#' @param varint the variable of interest. For instance if all other variables should be summarised at the same temporal resolution as this specified variable. It must correspond to variable or list in `dta`
#'
#' @return reduced/summarised and interpolated dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#' reduced_dta = reducePAM(PAM_data, interp = T)
#' head(reduced_dta)
#'
#' @importFrom zoo na.approx
#'
#' @export
reducePAM <- function(dta,
                         varint = c("pressure","light","pit","act",
                                    "temperature","gX","gY","gZ","mX", "mY","mZ")){

  PAM_data = dta

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

  new = Reduce(function(...) merge(..., all = T), PAM_data)
  index = which(!is.na(new[,which(colnames(new) %in% varint)]))
  to_change = which(!(colnames(new) %in% c(varint, "date")))

  test = do.call(cbind, lapply(to_change,
                        FUN = function(col){
                          if(any(is.na(new[,col]))){
                            first = which(!is.na(new[,col]))[1]
                            last = which(!is.na(new[,col]))[length(which(!is.na(new[,col])))]
                            new[first:last,col] <- zoo::na.approx(new[first:last,col])
                          } else {
                            median=c(new[index[1],col],
                                     unlist(lapply(1:(length(index)-1), FUN = function(x){median( new[(index[x]+1): index[x+1], col])})))
                            new[index,col] <- median
                          }
                          return(new[,col])
                        }))

  test=data.frame(new$date,  new[,varint], test)
  colnames(test) = colnames(new)
  test = test[complete.cases(test),]

  return(new)
}
