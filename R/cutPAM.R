#' Crop all sensor data to the same time
#'
#' @description Get rid of excess data. e.g. when a logger is kept in a rucksack or a lab before being downloaded.
#'
#' @param dta path where files are stored
#' @param start posicxt object for date that PAM data should start
#' @param end posicxt object for date that PAM data should end
#'
#' @return shortened PAM data
#'
#' @examples
#' #data(hoopoe)
#' #PAM_data=hoopoe
#' #str(PAM_data)
#'
#' #start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
#' #end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")
#'
#' #newPAM = cutPAM(PAM_data,start,end)
#'
#' @export
cutPAM <- function(dta, start, end){
  print("Error: This function is deprecated, use wrange_crop, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")


  # # for testing
  # # dta = PAM_data
  # # start = as.POSIXct("2015-08-01","%Y-%m-%d", tz="UTC")
  # # end = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
  #
  # test <- lapply(1:length(dta), function(x) {if(x>=2) {
  #   as.data.frame(dta[[x]])[(as.data.frame(dta[[x]])[,1] >= start & as.data.frame(dta[[x]])[,1] <= end),]
  # }else{ as.character(dta[[x]])}
  #   })
  # names(test) = names(dta)
  # return(test)
}
