#' Integrate classification from classifyPAM into PAM data
#'
#' @param PAM_data raw pam data see `data(bee_eater` for example
#' @param interp logical. whether or not to interpolate if there are NAs
#' @param keep_NA logical. Whether or not to keep NA
#'
#' @return merged and interpolated dataset
#'
#' @examples
#' data(bee_eater)
#' PAM_data = bee_eater
#' merged_dta = mergePAM(PAM_data, interp = T)
#' head(merged_dta)
#'
#' @importFrom zoo na.approx
#'
#' @export
mergePAM <- function(PAM_data, interp = TRUE, keep_NA=TRUE ){

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

  new = Reduce(function(...) merge(..., all=keep_NA), PAM_data)

  if(interp == TRUE){
    new[,2:ncol(new)] = na.approx(new[,2:ncol(new)])
  }

  return(new)
}
