#' calculateReliability: determine the reliability and SEM per Type
#'
#' @param mydata A dataframe containing columns ID, Type, Score (numeric)
#' @param n A vector containing for each Type the number of score or assessments assessments, e.g. averages, requirements.
#'
#' @return A list containing 2 vectors; one vector with the reliability coefficient of each Type, the other vector with the SEM values for each Type
#' @export
#'
#' @examples
#' rel <- calculateReliability(mydata, n=c("A"=3,"B"=3,C="2"))

calculateReliability <- function(mydata, n) {
  checkDatasets(mydata, n)
  varCovMatrix <- calculateVarCov(mydata, n)
  types <- sort(unique(mydata$Type))

  # initialize variables
  E_rho2_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))
  SEM_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))

  # determine the reliability and SEM per Type
  for(aType in types) {
    E_rho2_vector[aType, "value"] <- varCovMatrix$S_p[aType,aType]/(varCovMatrix$S_p[aType,aType] + varCovMatrix$S_delta[aType,aType])
    SEM_vector[aType, "value"] <- sqrt(varCovMatrix$S_delta[aType,aType])
  }

  return(list("E_rho2_vector"=E_rho2_vector,"SEM_vector"=SEM_vector))
}
