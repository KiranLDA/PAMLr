#' Import PAM data
#'
#' @description Imports and formats many datasets into one big nested list containing all the data from the different sensors. A subset of sensors can be selected using `measurements`.
#'
#' @param pathname path where files are stored
#' @param measurements a series of measurements logged by the PAM logger which are to be imported. Currently supports these file extentions: ".pressure", ".glf", ".gle",".acceleration", ".temperature", "AirTemperature", ".BodyTemperature" and ".magnetic"
#'
#' @return a list of measurements for the one individual
#'
#' @examples
#' #pathname = "your/path/here"
#' #measurements = c(".pressure", ".glf")
#' #PAM_data = create_import(pathname, measurements)
#' #str(PAM_data)
#' #plot(PAM_data$light$date[3000:5000], PAM_data$light$obs[3000:5000],
#' #type="l", xlab="Date", ylab="Light Intensity")
#'
#' @importFrom utils read.delim
#' @export
create_import <- function(pathname = pathname,
                      measurements = c(".pressure", ".glf", ".acceleration", ".temperature", ".magnetic")){
  dta=list()

  id = substring(list.files(pathname,pattern=measurements[1],full.names = FALSE),1,4)
  dta$id = id
  if(".pressure" %in% measurements){
    pressure = read.delim(list.files(pathname,pattern=".pressure",full.names = TRUE),skip=6,sep="",header = FALSE)
    pressure = as.data.frame(list(date=as.POSIXct(strptime(paste(pressure[,1],pressure[,2]),
                                                           tz="UTC",format="%d.%m.%Y %H:%M")),obs=pressure[,3]))
    dta$pressure = pressure
  }
  if(".glf" %in% measurements){
    light = read.delim(list.files(pathname,pattern=".glf",full.names = TRUE)[1],skip=6,sep="",header = FALSE)
    light = as.data.frame(list(date=as.POSIXct(strptime(paste(light[,1],light[,2]),
                                                        tz="UTC",format="%d.%m.%Y %H:%M")),obs=light[,3]))
    dta$light = light
  }
  if(".gle" %in% measurements){
    light = read.delim(list.files(pathname,pattern=".gle",full.names = TRUE)[1],skip=6,sep="",header = FALSE)
    light = as.data.frame(list(date=as.POSIXct(strptime(paste(light[,1],light[,2]),
                                                        tz="UTC",format="%d.%m.%Y %H:%M")),obs=light[,3]))
    dta$light = light
  }
  if(".acceleration" %in% measurements){
    acceleration = read.delim(list.files(pathname,pattern=".acceleration",full.names = TRUE),skip=6,sep="",header = FALSE)
    acceleration = as.data.frame(list(date=as.POSIXct(strptime(paste(acceleration[,1],acceleration[,2]),
                                                               tz="UTC",format="%d.%m.%Y %H:%M")),pit=acceleration[,3], act=acceleration[,4]))
    dta$acceleration = acceleration
  }
  if(".temperature" %in% measurements){
    temperature = read.delim(list.files(pathname,pattern=".temperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    temperature = as.data.frame(list(date=as.POSIXct(strptime(paste(temperature[,1],temperature[,2]),
                                                              tz="UTC",format="%d.%m.%Y %H:%M")),obs=temperature[,3]))
    dta$temperature = temperature
  }
  if(".AirTemperature" %in% measurements){
    temperature = read.delim(list.files(pathname,pattern=".AirTemperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    temperature = as.data.frame(list(date=as.POSIXct(strptime(paste(temperature[,1],temperature[,2]),
                                                              tz="UTC",format="%d.%m.%Y %H:%M")),obs=temperature[,3]))
    dta$temperature = temperature
  }
  if (".BodyTemperature" %in% measurements){
    bodytemperature = read.delim(list.files(pathname,pattern=".BodyTemperature",full.names = TRUE),skip=6,sep="",header = FALSE)
    bodytemperature = as.data.frame(list(date=as.POSIXct(strptime(paste(bodytemperature[,1],bodytemperature[,2]),
                                                                  tz="UTC",format="%d.%m.%Y %H:%M")),obs=bodytemperature[,3]))
    dta$bodytemperature = bodytemperature

  }
  if(".magnetic" %in% measurements){
    magnetic = read.delim(list.files(pathname,pattern=".magnetic",full.names = TRUE),skip=6,sep="",header = FALSE)
    magnetic = as.data.frame(list(date=as.POSIXct(strptime(paste(magnetic[,1],magnetic[,2]),
                                                           tz="UTC",format="%d.%m.%Y %H:%M")),
                                  gX=magnetic[,4],gY=magnetic[,5],gZ=magnetic[,6],
                                  mX=magnetic[,7],mY=magnetic[,8],mZ=magnetic[,9]))
    dta$magnetic = magnetic
  }

  # tryCatch({
  #   dta$light = light
  # }, error = function(e) return("no light data"))
  # tryCatch({
  #   dta$pressure = pressure
  # }, error = function(e) return("no pressure data"))
  # tryCatch({
  #   dta$acceleration = acceleration
  # }, error = function(e) return("no acceleration data"))
  # tryCatch({
  #   dta$temperature = temperature
  # }, error = function(e) return("no temperature data"))
  # tryCatch({
  #   dta$bodytemperature = bodytemperature
  # }, error = function(e) return("no body temperature data"))
  # tryCatch({
  #   dta$magnetic = magnetic
  # }, error = function(e) return("no magnetic data"))

  return(dta)
}
