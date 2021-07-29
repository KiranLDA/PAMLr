#' Derive classification data for soaring birds
#'
#' @description This function takes data, typically a dataframe of events, which are classified into different behavioural states using different algorithms.
#'
#' @param dta data to be classified, can be anything
#' @param method method for classifying data, currently support "hmm" for hidden markov model and "kmeans" for kmeans clustering
#' @param states number of states to classify the data into
#' @param family By default `gaussian()`. Parameter only used in the hmm method. Must be the same as the parameters used by `depmixS4::depmix`
#' @param ... any additional inputs for depmixs4::depmix, stats::kmeans, cluster::diana, cluster::diana  or EMbC::embc functions, depending on method selected
#'
#' @return the data's classification based on the chosen algorithm
#'
#' @references Forgy, E. W. (1965). Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics, 21, 768–769.
#' @references Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108. doi: 10.2307/2346830.
#' @references Lloyd, S. P. (1957, 1982). Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory, 28, 128–137.
#' @references MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press.
#' @references Ingmar Visser and Maarten Speekenbrink (2010). depmixS4: An R Package for Hidden Markov Models. Journal of Statistical Software, 36(7), p. 1-21.
#' @references Lawrence R. Rabiner (1989). A tutorial on hidden Markov models and selected applications in speech recognition. Proceedings of IEEE, 77-2, p. 267-295.
#' @references Kaufman, L. and Rousseeuw, P.J. (1990). Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.
#' @references Anja Struyf, Mia Hubert and Peter J. Rousseeuw (1996) Clustering in an Object-Oriented Environment. Journal of Statistical Software 1. http://www.jstatsoft.org/v01/i04
#' @references Struyf, A., Hubert, M. and Rousseeuw, P.J. (1997). Integrating Robust Clustering Techniques in S-PLUS, Computational Statistics and Data Analysis, 26, 17–37.
#' @references Lance, G.N., and W.T. Williams (1966). A General Theory of Classifactory Sorting Strategies, I. Hierarchical Systems. Computer J. 9, 373–380.
#' @references Belbin, L., Faith, D.P. and Milligan, G.W. (1992). A Comparison of Two Approaches to Beta-Flexible Clustering. Multivariate Behavioral Research, 27, 417–433.
#' @references Gower, J. C. (1971) A general coefficient of similarity and some of its properties, Biometrics 27, 857–874.
#' @references Garriga, J., Palmer, J.R., Oltra, A. and Bartumeus, F., 2016. Expectation-maximization binary clustering for behavioural annotation. PLoS One, 11(3), p.e0151984.
#' @references Garriga, J., Palmer, J.R.B., Oltra, A. and Bartumeus, F., 2014. EMbC: expectation-maximization binary clustering. arXiv preprint arxiv:1503.04059, 1.
#'
#' @examples
#'
#' #######################################################
#' #  data prep
#' #######################################################
#'
#' #data(bee_eater)
#' #start = as.POSIXct("2015-07-01","%Y-%m-%d", tz="UTC")
#' #end = as.POSIXct("2016-06-01","%Y-%m-%d", tz="UTC")
#' #PAM_data = cutPAM(bee_eater, start, end)
#'
#' # twl = GeoLight::twilightCalc(PAM_data$light$date, PAM_data$light$obs,
#' #LightThreshold = 2, ask = FALSE)
#' #availavariable = c("pressure", "light", "acceleration")
#'
#' #TOclassify = pamPREP(PAM_data,
#' #                      method= "flap",
#' #                      twl = twl)
#'
#' #TOclassify = TOclassify[complete.cases(TOclassify),]
#'
#' #######################################################
#' # k-means example
#' #######################################################
#'
#' #classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#' #                             states=2, "kmeans")$cluster
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #to =TOclassify$end,
#' #classification = classification,
#' #addTO = PAM_data$pressure)
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     type="l")
#' #points(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #       col= pressure_classification+1,
#' #       pch=16)
#'
#' #######################################################
#' # HMM example
#' #######################################################
#'
#' #classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )]
#' #                             #TOclassify$night_P_diff +
#' #                             #TOclassify$cum_altitude_change
#' #                             ,
#' #                             states=2, "hmm")$cluster
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #to =TOclassify$end,
#' #classification = classification,
#' #addTO = PAM_data$pressure)
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     type="l")
#' #points(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #       col= pressure_classification+1,
#' #       pch=16)
#'
#'
#' #######################################################
#' # EMBC example
#' #######################################################
#'
#' #classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#' #                             "embc")
#'
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #                                             to =TOclassify$end,
#' #                                             classification = classification$cluster,
#' #                                             addTO = PAM_data$pressure)
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     type="l")
#' #points(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #       col= classification$output@C[pressure_classification+1],
#' #       pch=16)
#'
#'
#' #######################################################
#' # agnes example
#' #######################################################
#'
#' #classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#' #                             states = 2,
#' #                            "agnes")
#'
#' #plot(classification$output, main="agnes", which.plot = 2)
#'
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #                                             to =TOclassify$end,
#' #                                             classification = classification$cluster,
#' #                                             addTO = PAM_data$pressure)
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     type="l")
#' #points(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #       col= pressure_classification+1,
#' #       pch=16)
#'
#' #######################################################
#' # diana example
#' #######################################################
#'
#' #classification = classifyPAM(TOclassify[,c("cum_altitude_change", "night_P_diff" )],
#' #                             states = 2,
#' #                             "diana")
#'
#' #plot(classification$output, which.plot = 2, main="diana")
#'
#' #pressure_classification = classification2PAM(from = TOclassify$start,
#' #                                             to =TOclassify$end,
#' #                                             classification = classification$cluster,
#' #                                             addTO = PAM_data$pressure)
#'
#' #plot(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #     type="l")
#' #points(PAM_data$pressure$date, PAM_data$pressure$obs,
#' #       col= pressure_classification+1,
#' #       pch=16)
#'
#'
#'
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom stats kmeans gaussian cutree as.formula binomial Gamma inverse.gaussian poisson quasipoisson quasibinomial quasi
#' @importFrom EMbC embc
#' @importFrom cluster daisy agnes diana
#'
#' @export
classifyPAM <- function(dta ,
                        method = c("kmeans","hmm", "embc", "agnes", "diana"),
                        states = 2,
                        family = stats::gaussian(),
                        ...){
  print("Error: This function is deprecated, use classify_summary_statistics, or install v.1.0 of PAMLr by running devtools::install_github('KiranLDA/PAMLr', ref = 'v.1.0')")


  # if(any(is.na(dta))){
  #   stop('NAs are present in the dataset and should be removed')
  #   }
  #
  # classification <- list()
  # classification$method = method
  #
  # if (method == "hmm"){
  #   if(is.null(dim(dta))) {
  #     hmm <- depmix(dta ~ 1, family = family,
  #                   nstates = states, ntimes=length(dta), ... )
  #   } else {
  #     hmm <- depmix(#lapply( 1:ncol(dta), function(x)  as.formula(dta[,x] ~ 1) ) ,
  #                   lapply( 1:ncol(dta), function(x)  as.formula(paste0("dta[,",as.character(x),"] ~ 1")) ) ,
  #                   family = lapply(1:ncol(dta), function(x) family),
  #                   nstates = states, ntimes=nrow(dta), ... )
  #   }
  #
  #   hmmfit <- fit(hmm, verbose = FALSE)
  #   classification$output <- posterior(hmmfit)
  #   classification$cluster <- classification$output$state
  #
  # }
  #
  # if (method == "kmeans"){
  #   classification$output <- kmeans(dta,centers=states,...)
  #   classification$cluster <- classification$output$cluster
  # }
  #
  # if(method == "embc"){
  #   mybc <- embc(as.matrix(dta), ...)
  #   classification$cluster <- mybc@A
  #   classification$output <- mybc
  # }
  #
  # if(method == "agnes"){
  #   x <- daisy(dta, metric = c("euclidean"),
  #              stand = FALSE, type = list())
  #   classification$output <- agnes(x,method = "ward", keep.diss = TRUE , ... )
  #   classification$cluster <- cutree(classification$output, k = states)
  # }
  # if(method == "diana"){
  #   x <- daisy(dta, metric = c("euclidean"),
  #              stand = FALSE, type = list())
  #   classification$output <- diana(x, ... )
  #   classification$cluster <- cutree(classification$output, k = states)
  # }
  # # if (method == "pca"){
  # #   pca <- prcomp(x=dta, retx=TRUE, center=FALSE, scale.=F)
  # #   plot(pca )
  # #   summary(pca )
  # #   biplot(pca )
  # #
  # # }
  #
  # return(classification)
}
