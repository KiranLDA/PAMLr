#' Merges all sensors into one
#'
#' @description This function takesthe typical PAM_data input, which is a nested list of different sensor data, all formatted at different time resolutions, and merges them all into one big table. By default all times are kept, and not interpolated.
#'
#' @param dta raw pam data see `data(bee_eater` for example
#' @param all logical. Default TRUE. Whether or not to keep NAs (i.e. all the datasets)
#' @param interp logical. Default FALSE. whether or not to interpolate if there are NAs. if all = FALSE then interp is not used.
#'
#' @return merged and interpolated dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#' merged_dta = create_merge(PAM_data, interp = TRUE)
#' head(merged_dta)
#'
#' @importFrom zoo na.approx
#'
#' @export
create_merge <- function(dta,
                     all = TRUE,
                     interp = FALSE
){

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

  rownames(new) = c()

  return(new)
}
