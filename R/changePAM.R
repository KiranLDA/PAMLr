#' Changepoint analysis
#'
#' @description This function implements a changepoint analysis to find changes in variance, mean or both. By default it is set to look for changes in the pressure data to identify migration periods, however it can be altered to any form of changepoint analysis in the package changepoint.
#'
#' @param dta data to be classified (vector)
#' @param cpt.method method for classifying data, currently support "mean","variance", "meanvar", use ?changepoint::cpt.mean ?changepoint::cpt.var or ?changepoint::cpt.meanvar for additional parameters
#' @param method changepoint method used in package changepoint. see changepoint::cpt.mean, changepoint::cpt.var and changepoint::cpt.meanvar for details
#' @param penalty See changepoint  package for details. Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic", "Manual" and "CROPS" penalties. If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter. If CROPS is specified, the penalty range is contained in the pen.value parameter; note this is a vector of length 2 which contains the minimum and maximum penalty value. Note CROPS can only be used if the method is "PELT". The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
#' @param pen.value	See changepoint package for details. Tthe theoretical type I error e.g.0.05 when using the Asymptotic penalty. A vector of length 2 (min,max) if using the CROPS penalty. The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use. Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters
#' @param ... any additional inputs for changepoint::cpt.mean,  changepoint::cpt.var or changepoint::cpt.meanvar
#'
#' @return Changepoints in the data.
#'
#' @references Killick, R. and Eckley, I., 2014. changepoint: An R package for changepoint analysis. Journal of statistical software, 58(3), pp.1-19.
#'
#' @examples
#'
#' ## Import and crop PAM data
#' #data("swift")
#' #start = as.POSIXct("2016-09-01","%Y-%m-%d", tz="UTC")
#' #end = as.POSIXct("2017-04-15","%Y-%m-%d", tz="UTC")
#' #swift = cutPAM(swift, start, end)
#' #PAM_data = swift
#'
#' # data(bee_eater)
#' # start = as.POSIXct("2015-08-01","%Y-%m-%d", tz="UTC")
#' # end = as.POSIXct("2016-06-01","%Y-%m-%d", tz="UTC")
#' # PAM_data = cutPAM(bee_eater, start, end)
#'
#' #changepoints  = changePAM(PAM_data$pressure$obs)
#'
#' # plot the timeseries with the changepoint
#' #par(mfrow=c(2,1))
#' #plot(PAM_data$pressure$obs, type="l")
#' #abline(v=changepoints$changepoints, col="red",lwd=2)
#'
#' ## plot using changepoint package output
#' #changepoint::plot(changepoints$output, cpt.width=3)
#'
#' @importFrom changepoint cpt.mean cpt.var cpt.meanvar cpts
#'
#' @export
changePAM <- function(dta,
                      cpt.method = "meanvar",
                      method='PELT',
                      penalty='Manual',
                      pen.value='100*log(n)',
                      ...){
  print("Error: This function is deprecated, use classify_changepoint in latest version of PAMLr which can be installed by running devtools::install_github('KiranLDA/PAMLr')")
  # if(any(is.na(dta))){
  #   stop('NAs are present in the dataset and should be removed')
  # }
  # changepoints = list()
  # if(cpt.method == "meanvar"){
  #   changepoints$output  = cpt.meanvar(dta,
  #                                      method=method,
  #                                      penalty=penalty,
  #                                      pen.value=pen.value,
  #                                      ...)
  # }
  # if(cpt.method == "mean"){
  #   changepoints$output  = cpt.mean(dta,
  #                                   method=method,
  #                                   penalty=penalty,
  #                                   pen.value=pen.value,
  #                                   ...)
  # }
  # if(cpt.method == "variance"){
  #   changepoints$output  = cpt.var(dta,
  #                                  method=method,
  #                                  penalty=penalty,
  #                                  pen.value=pen.value,
  #                                  ...)
  # }
  # changepoints$changepoints = cpts(changepoints$output)
  #
  # return(changepoints)
}
