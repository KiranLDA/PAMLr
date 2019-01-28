#' Derive classification data for soaring birds
#'
#' @param dta data to be classified, can be anything
#' @param method method for classifying data, currently support "hmm" for hidden markov model and "kmeans" for kmeans clustering
#' @param states number of states to classify the data into
#' @param ... any additional inputs for depmixs4::depmix or or stats::kmeans functions, depending on method selected
#'
#' @return the data's classification based on the chosen algorithm
#'
#' @examples
#' data(hoopoe)
#' PAM_data=hoopoe
#'
#' twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#' LightThreshold = 2, ask = FALSE)
#' availavariable = c("pressure", "light", "acceleration")
#'
#' TOclassify = soarPREP(dta = PAM_data, availavariable = availavariable, twl = twl)
#'
#' classification = classifyPAM(TOclassify$night_P_diff, states=3, "hmm")$state
#' pressure_classification = classification2PAM(from = TOclassify$start,
#' to =TOclassify$end,
#' classification = classification,
#' addTO = PAM_data$pressure)
#'
#' plot(PAM_data$pressure$date, PAM_data$pressure$date, col= pressure_classification+1, type="o")
#'
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom stats kmeans gaussian
#'
#' @export
classifyPAM <- function(dta ,
                        method = c("kmeans","hmm"),
                        states = 2,
                        ...){

  if (method == "hmm"){
    hmm <- depmix(dta ~ 1, family = gaussian(), nstates = states, ntimes=length(dta),... )
    hmmfit <- fit(hmm, verbose = FALSE)
    classification <- posterior(hmmfit)
    classification$cluster <- classification$state

  }
  if (method == "kmeans"){
    classification <- kmeans(dta,centers=states,...)
  }
  classification$method = method
  return(classification)
}
