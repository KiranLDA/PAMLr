#' Calculate error and create a confusion matrix
#'
#' @param reference Reference dataset wich classification is to be compared to
#' @param classified Classification output to compare with a reference dataset
#'
#' @return a confusion matrix
#'
#' @examples
#'
#' # Get hoopoe data and crop where appropriate
#' data("hoopoe")
#' start = as.POSIXct("2016-08-01","%Y-%m-%d", tz="UTC")
#' end = as.POSIXct("2017-05-15","%Y-%m-%d", tz="UTC")
#' PAM_data = cutPAM(hoopoe, start, end)
#'
#' # Generate a classification from activity
#' activity_classification = classifyFLAP(PAM_data$acceleration, period = 10)
#' reference = activity_classification$classification
#' reference[reference != activity_classification$migration] = "Not Migrating"
#' reference[(activity_classification$classification ==
#'           activity_classification$high_movement)] = "Flying"
#' reference[reference == activity_classification$migration] = "Migrating"
#'
#' # Generate a very simple pressure classification based on pressure changes
#' classified  = ifelse( abs(diff(PAM_data$pressure$obs)) >2, "Migrating", "Not Migrating")
#'
#' # Remove dates from activity which are not also present in pressure
#' to_keep = which(PAM_data$acceleration$date %in% PAM_data$pressure$date)
#' reference = reference[to_keep]
#' reference = reference[-1]
#'
#' confusionMAT(reference, classified)
#'
#' @export
confusionMAT <- function(reference, classified){
  states = base::unique(c(reference, classified))
  nstates = length(states)
  confusion = matrix(0, nstates, nstates)
  colnames(confusion) <- unlist(lapply(1:nstates, function(x) paste("Ref",states[x])))
  rownames(confusion) <- unlist(lapply(1:nstates, function(x) paste("Pred",states[x])))

  ref = lapply(1:nstates, function(x) which(reference %in% states[x]))
  cla = lapply(1:nstates, function(x) which(classified %in% states[x]))

  for (row in 1:nrow(confusion)){
    for (col in 1:ncol(confusion)){
      confusion[row,col] =  length(which(cla[[row]] %in% ref[[col]]))
    }
  }

  Col_Total = apply(confusion,2, sum)
  confusion <- rbind(confusion, Col_Total)
  Row_Total =  apply(confusion,1, sum)
  confusion <- cbind(confusion, Row_Total)

  # Error calculations
  Commission_Error = unlist(lapply(1:nstates, function(x) sum(confusion[x, which(1:nstates != x)])/ (confusion[x,"Row_Total"]+0.000001)))
  Commission_Error = c(Commission_Error, NA)
  confusion = cbind(confusion, Commission_Error)

  Omission_Error = unlist(lapply(1:nstates, function(x) sum(confusion[which(1:nstates != x),x])/ (confusion["Col_Total",x]+0.0000001)))
  Omission_Error = c(Omission_Error, NA, NA)
  confusion = rbind(confusion, Omission_Error)

  # Accuracy calculations
  Producers_accuracy = unlist(lapply(1:nstates, function(x) confusion[x,x]/(Col_Total[x]+0.000001)))
  Producers_accuracy = c(Producers_accuracy, NA, NA)
  Users_accuracy = unlist(lapply(1:nstates, function(x) confusion[x,x]/(Row_Total[x]+0.000001)))
  Users_accuracy = c(Users_accuracy, NA, NA, NA)

  #Add accuracy to table
  confusion = rbind(confusion, Producers_accuracy)
  confusion = cbind(confusion, Users_accuracy)


  # Add total accuracy
  Total_accuracy <- rep_len(NA, nrow(confusion))
  confusion <- cbind(confusion, Total_accuracy)
  Total_accuracy <- rep_len(NA, ncol(confusion))
  confusion <- rbind(confusion, Total_accuracy)
  confusion["Total_accuracy","Total_accuracy"] = sum(diag(confusion)[1:nstates])/diag(confusion)[nstates+1]

  #kappa coefficient calculation
  x = 1
  TP = confusion[x,x]
  FN = sum(confusion[x, which(1:nstates != x)])
  FP = sum(confusion[which(1:nstates != x), x])
  TN = sum(confusion[which(1:nstates != x),which(1:nstates != x)])
  kappa = ((TN+FP)*(TN+FN)+(FN+TP)*(FP+TP))/(confusion["Col_Total","Row_Total"]^2)

  Kappa_Coeff <- rep_len(NA, nrow(confusion))
  confusion <- cbind(confusion, Kappa_Coeff)
  Kappa_Coeff <- rep_len(NA, ncol(confusion))
  confusion <- rbind(confusion, Kappa_Coeff)
  confusion["Kappa_Coeff","Kappa_Coeff"] = kappa


  print(confusion)
  return(confusion)

}
