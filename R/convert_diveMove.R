#' convert diveMove input into pamlr input
#'
#' @description Converts  diveMove input into pamlr input
#'
#' @param input diveMove data
#' @param id individual identifyer for tracked animal e.g. leg tag number
#' @param measurements a series of measurements logged by the dtr logger which are to be imported. Currently supports: "light", "depth","temperature"
#'
#' @return a list of measurements for the one individual
#'
#' @examples
#' #install.packages("diveMove")
#' #library(diveMove)
#' #zz <- system.file(file.path("data", "dives.csv"),
#' #                  package="diveMove", mustWork=TRUE)
#' #(sealX <- readTDR(zz, speed=TRUE, sep=";", na.strings="", as.is=TRUE))
#'
#' #PAM_data = convert_diveMove(sealX, id="sealX")
#'
#' @export
convert_diveMove <- function(input, id= 0,  measurements = c("light", "depth", "temperature")){

  dta = list()
  dta$id = id

  if ("light" %in% measurements){
    dta$light$date <- input@time
    dta$light$obs <- input@concurrentData$light
    dta$light <- as.data.frame(dta$light)
  }

  if ("depth" %in% measurements){
    dta$pressure$date <- input@time
    dta$pressure$obs <- calculate_water_pressure(input@depth)
    dta$pressure <- as.data.frame(dta$pressure)
  }

  if ("temperature" %in% measurements){
    dta$temperature$date <- input@time
    dta$temperature$obs <- input@concurrentData$temperature
    dta$temperature <- as.data.frame(dta$temperature)
  }
  return(dta)
}
