#' Import PAM data
#'
#' @param pathname path where files are stored
#' @param measurements a series of measurements logged by the PAM logger which are to be imported. Currently supports these file extentions: ".pressure", ".glf", ".gle",".acceleration", ".temperature" and ".magnetic"
#'
#' @return a list of measurements for the one individual
#'
#' @examples
#' pathname = "R:/40 Data/20 Geolocator/10 Raw data/UpuEpoCH16/16AJ_20170809"
#' measurements = c(".pressure", ".glf")
#' PAM_data = importPAM(pathname, measurements)
#' str(PAM_data)
#' plot(PAM_data$light$date[3000:5000], PAM_data$light$obs[3000:5000],
#' type="l", xlab="Date", ylab="Light Intensity")
#'
#'
#' @export
importPAM <- function(pathname = pathname,measurements = c(".pressure", ".glf", ".acceleration", ".temperature", ".magnetic")){

  id = substring(list.files(pathname,pattern=".pressure",full.names=F),1,4)

  if(".pressure" %in% measurements){
    pressure = utils::read.delim(list.files(pathname,pattern=".pressure",full.names=T),skip=6,sep="",header=F)
    pressure = as.data.frame(list(date=as.POSIXct(strptime(paste(pressure[,1],pressure[,2]),tz="UTC",format="%d.%m.%Y %H:%M")),obs=pressure[,3]))
  }
  if((".glf" %in% measurements) | (".gle" %in% measurements)){
    light = utils::read.delim(list.files(pathname,pattern=".glf",full.names=T),skip=6,sep="",header=F)
    light = as.data.frame(list(date=as.POSIXct(strptime(paste(light[,1],light[,2]),tz="UTC",format="%d.%m.%Y %H:%M")),obs=light[,3]))
  }
  if(".acceleration" %in% measurements){
    acceleration = utils::read.delim(list.files(pathname,pattern=".acceleration",full.names=T),skip=6,sep="",header=F)
    acceleration = as.data.frame(list(date=as.POSIXct(strptime(paste(acceleration[,1],acceleration[,2]),tz="UTC",format="%d.%m.%Y %H:%M")),pit=acceleration[,3], act=acceleration[,4]))
  }
  if(".temperature" %in% measurements){
    temperature = utils::read.delim(list.files(pathname,pattern=".temperature",full.names=T),skip=6,sep="",header=F)
    temperature = as.data.frame(list(date=as.POSIXct(strptime(paste(temperature[,1],temperature[,2]),tz="UTC",format="%d.%m.%Y %H:%M")),obs=temperature[,3]))
  }
  if(".magnetic" %in% measurements){
    magnetic = utils::read.delim(list.files(pathname,pattern=".magnetic",full.names=T),skip=6,sep="",header=F)
    magnetic = as.data.frame(list(date=as.POSIXct(strptime(paste(magnetic[,1],magnetic[,2]),tz="UTC",format="%d.%m.%Y %H:%M")),gX=magnetic[,4],gY=magnetic[,5],gZ=magnetic[,6],mX=magnetic[,7],mY=magnetic[,8],mZ=magnetic[,9]))
  }
  dta=list()
  dta$id=id
  tryCatch({
    dta$pressure = pressure
    }, error = function(e) return("no pressure data"))
  tryCatch({
    dta$light = light
  }, error = function(e) return("no light data"))
  tryCatch({
    dta$acceleration = acceleration
  }, error = function(e) return("no acceleration data"))
  tryCatch({
    dta$temperature = temperature
  }, error = function(e) return("no temperature data"))
  tryCatch({
    dta$magnetic = magnetic
  }, error = function(e) return("no magnetism data"))

  return(dta)
}
