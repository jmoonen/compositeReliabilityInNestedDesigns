calculateVarCov <- function(mydata, n){
  ID = Type = Score = NULL

  types <- sort(unique(mydata$Type))

    # Estimate variance and covariance components of assessee p S_p and mean assessment scores i nested in assessees S_iINp
  combinedDataset <- mydata %>% dplyr::group_by(ID, Type) %>% dplyr::summarise(Score = mean(Score))
  typeDataset <- tidyr::spread(combinedDataset, Type, Score) %>% dplyr::ungroup() %>% dplyr::select(-ID)
  S_p <- stats::cov(typeDataset)
  diag(S_p) <- 0

  S_iINp <- matrix(0, nrow=length(types), ncol=length(types), dimnames=list(types, types))
  S_Delta <- matrix(0, nrow=length(types), ncol=length(types), dimnames=list(types, types))

  for(aType in types) {
    dataPerType <- dplyr::filter(mydata,Type==aType)

    res <- GstudyPerType(dataPerType)
    S_p[aType,aType] = res[c("ID"),]$vcov
    S_iINp[aType,aType] = res[c("Residual"),]$vcov
  }

  for(aType in types) {
    S_Delta[aType,aType] = S_iINp[aType,aType] / n[aType]
  }

  return (list("S_p"=S_p, "S_iINp"=S_iINp, "S_Delta"=S_Delta))
}
