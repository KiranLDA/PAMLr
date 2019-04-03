#' Derive classification data for soaring birds
#'
#' @param dta data to be classified, can be anything
#' @param method method for classifying data, currently support "hmm" for hidden markov model and "kmeans" for kmeans clustering
#' @param states number of states to classify the data into
#' @param ... any additional inputs for depmixs4::depmix, stats::kmeans, cluster::diana, cluster::diana  or EMbC::embc functions, depending on method selected
#'
#' @return the data's classification based on the chosen algorithm
#'
#' @examples
#'
#' #######################################################
#' #  data prep
#' #######################################################
#'
#' data(bee_eater)
#' start = as.POSIXct("2015-07-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2016-06-01","%Y-%m-%d", tz="UTC")
#' PAM_data = cutPAM(bee_eater, start, end)
#'
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#' LightThreshold = 2, ask = FALSE)
#' availavariable = c("pressure", "light", "acceleration")
#'
#' TOclassify = soarPREP(dta = PAM_data, availavariable = availavariable, twl = twl)
#' TOclassify = TOclassify[complete.cases(TOclassify),]
#'
#'  #######################################################
#' # k-means example
#' #######################################################
#'
#' classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#'                              states=2, "kmeans")$cluster
#' pressure_classification = classification2PAM(from = TOclassify$start,
#' to =TOclassify$end,
#' classification = classification,
#' addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      type="l")
#' points(PAM_data$pressure$date, PAM_data$pressure$obs,
#'        col= pressure_classification+1,
#'        pch=16)
#'
#'
#' #######################################################
#' # HMM example
#' #######################################################
#'
#' classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )]
#'                              #TOclassify$night_P_diff +
#'                              #TOclassify$cum_altitude_change
#'                              ,
#'                              states=2, "hmm")$cluster
#' pressure_classification = classification2PAM(from = TOclassify$start,
#' to =TOclassify$end,
#' classification = classification,
#' addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      type="l")
#' points(PAM_data$pressure$date, PAM_data$pressure$obs,
#'        col= pressure_classification+1,
#'        pch=16)
#'
#'
#' #######################################################
#' # EMBC example
#' #######################################################
#'
#' classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#'                              "embc")
#'
#' pressure_classification = classification2PAM(from = TOclassify$start,
#'                                              to =TOclassify$end,
#'                                              classification = classification$cluster,
#'                                              addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      type="l")
#' points(PAM_data$pressure$date, PAM_data$pressure$obs,
#'        col= classification$output@C[pressure_classification+1],
#'        pch=16)
#'
#'
#' #######################################################
#' # agnes example
#' #######################################################
#'
#' classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#'                              states = 2,
#'                              "agnes")
#'
#' plot(classification$output, main="agnes", which.plot = 2)
#'
#' pressure_classification = classification2PAM(from = TOclassify$start,
#'                                              to =TOclassify$end,
#'                                              classification = classification$cluster,
#'                                              addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      type="l")
#' points(PAM_data$pressure$date, PAM_data$pressure$obs,
#'        col= pressure_classification+1,
#'        pch=16)
#'
#' #######################################################
#' # diana example
#' #######################################################
#'
#' classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#'                              states = 2,
#'                              "diana")
#'
#' plot(classification$output, which.plot = 2, main="diana")
#'
#' pressure_classification = classification2PAM(from = TOclassify$start,
#'                                              to =TOclassify$end,
#'                                              classification = classification$cluster,
#'                                              addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#'      type="l")
#' points(PAM_data$pressure$date, PAM_data$pressure$obs,
#'        col= pressure_classification+1,
#'        pch=16)
#'
#'
#'
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom stats kmeans gaussian cutree as.formula
#' @importFrom EMbC embc
#' @importFrom cluster daisy agnes diana
#'
#' @export
classifyPAM <- function(dta ,
                        method = c("kmeans","hmm", "embc"),
                        states = 2,
                        ...){

  if(any(is.na(dta))){
    stop('NAs are present in the dataset and should be removed')
    }

  classification <- list()
  classification$method = method

  if (method == "hmm"){
    if(is.null(dim(dta))) {
      hmm <- depmix(dta ~ 1, family = gaussian(),
                    nstates = states, ntimes=length(dta), ... )
    } else {
      hmm <- depmix(#lapply( 1:ncol(dta), function(x)  as.formula(dta[,x] ~ 1) ) ,
                    lapply( 1:ncol(dta), function(x)  as.formula(paste0("dta[,",as.character(x),"] ~ 1")) ) ,
                    family = lapply(1:ncol(dta), function(x) gaussian()),
                    nstates = states, ntimes=nrow(dta), ... )
    }

    hmmfit <- fit(hmm, verbose = FALSE)
    classification$output <- posterior(hmmfit)
    classification$cluster <- classification$output$state

  }

  if (method == "kmeans"){
    classification$output <- kmeans(dta,centers=states,...)
    classification$cluster <- classification$output$cluster
  }

  if(method == "embc"){
    mybc <- embc(as.matrix(dta), ...)
    classification$cluster <- mybc@A
    classification$output <- mybc
  }

  if(method == "agnes"){
    x <- daisy(dta, metric = c("euclidean"),
               stand = FALSE, type = list())
    classification$output <- agnes(x,method = "ward", keep.diss = TRUE , ... )
    classification$cluster <- cutree(classification$output, k = states)
  }
  if(method == "diana"){
    x <- daisy(dta, metric = c("euclidean"),
               stand = FALSE, type = list())
    classification$output <- diana(x, ... )
    classification$cluster <- cutree(classification$output, k = states)
  }
  if (method == "pca"){
    pca <- prcomp(x=dta, retx=TRUE, center=FALSE, scale.=F)
    plot(pca )
    summary(pca )
    biplot(pca )

  }

  return(classification)
}
