#' calculateVarCov: Estimate variance and covariance components of assessee p S_p and mean assessment scores i nested in assessees S_iINp,
#' and determine the error scores S_delta
#'
#' @param mydata A dataframe containing columns ID, Type, Score (numeric)
#' @param n A vector containing for each Type the number of score or assessments assessments, e.g. averages, requirements.
#'
#' @return A list containing the observed variances, covariances and errors scores
#' @export
#'
#' @examples
#' varcov <- calculateVarCov(mydata, c("A"=3, "B"=3, "C"=2))
#' varcov$S_p
#' varcov$S_iINp
#' varcov$S_delta

calculateVarCov <- function(mydata, n){
  # Check that the input data is valid
  checkDatasets(mydata, n, NULL)

  ID = Type = Score = NULL

  types <- sort(unique(mydata$Type))

  # Estimate variance and covariance components of assessee p S_p and mean assessment scores i nested in assessees S_iINp
  combinedDataset <- mydata %>% dplyr::group_by(ID, Type) %>% dplyr::summarise(Score = mean(Score))
  typeDataset <- tidyr::spread(combinedDataset, Type, Score) %>% dplyr::ungroup() %>% dplyr::select(-ID)
  S_p <- stats::cov(typeDataset)
  diag(S_p) <- 0

  S_iINp <- matrix(0, nrow=length(types), ncol=length(types), dimnames=list(types, types))
  S_delta <- matrix(0, nrow=length(types), ncol=length(types), dimnames=list(types, types))

  for(aType in types) {
    dataPerType <- dplyr::filter(mydata,Type==aType)

    res <- GStudyPerType(dataPerType)
    S_p[aType,aType] = res[c("ID"),]$vcov
    S_iINp[aType,aType] = res[c("Residual"),]$vcov
  }

  for(aType in types) {
    S_delta[aType,aType] = S_iINp[aType,aType] / as.numeric(n[aType])
  }

  return (list("S_p"=S_p, "S_iINp"=S_iINp, "S_delta"=S_delta))
}
