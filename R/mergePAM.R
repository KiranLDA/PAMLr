#' merges all the data into one table for interpolation
#'
#' @param dta raw pam data see `data(bee_eater` for example
#' @param interp logical. Default TRUE. whether or not to interpolate if there are NAs
#' @param all logical. Default TRUE. Whether or not to keep NAs (i.e. all the datasets)
#'
#' @return merged and interpolated dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#' merged_dta = mergePAM(PAM_data, interp = TRUE)
#' head(merged_dta)
#'
#' @importFrom zoo na.approx
#'
#' @export
mergePAM <- function(dta,
                     interp = TRUE,
                     all = TRUE){

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

  new = Reduce(function(...) merge(..., all = all), PAM_data)

  if(interp == TRUE){
    new[,2:ncol(new)] = na.approx(new[,2:ncol(new)])
  }

  return(new)
}
