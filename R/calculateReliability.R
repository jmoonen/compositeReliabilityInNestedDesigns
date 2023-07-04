calculateReliability <- function(types, mydata, n) {
  optSemCompRel=NULL
  varCovMatrix <- calculateVarCov(mydata, n)

  # initialize variables
  #UniverseScore_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))
  #ErrorScore_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))
  E_rho2_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))
  SEM_vector <- matrix(0, nrow=length(types), ncol=1, dimnames=list(types, "value"))

  # determine the reliability and SEM per Type
  for(aType in types) {
    #UniverseScore_vector[aType, "value"] <- S_p[aType,aType]
    #ErrorScore_vector[aType, "value"] <- S_Delta[aType,aType]
    E_rho2_vector[aType, "value"] <- varCovMatrix$S_p[aType,aType]/(varCovMatrix$S_p[aType,aType] + varCovMatrix$S_Delta[aType,aType])
    SEM_vector[aType, "value"] <- sqrt(varCovMatrix$S_Delta[aType,aType])
  }

  return(list("E_rho2_vector"=E_rho2_vector,"SEM_vector"=SEM_vector))
}
