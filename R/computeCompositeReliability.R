computeCompositeReliability <- function(mydata, n, weights, optimizeSEM) {
  varCovMatrix <- calculateVarCov(mydata, n)

  types <- sort(unique(mydata$Type))

  if(optimizeSEM) {
      weights[types[1]] = 1/(varCovMatrix$S_Delta[types[1],types[1]] * sum(diag(1/varCovMatrix$S_Delta)))
      for(t in 2:length(types)) {
        weights[types[t]] = weights[types[1]] * varCovMatrix$S_Delta[types[1],types[1]]/varCovMatrix$S_Delta[types[t],types[t]]
      }
  }

  weightMatrix <- weights %*% t(weights)

  w_S_p <- varCovMatrix$S_p * weightMatrix
  w_S_Delta <- varCovMatrix$S_Delta * weightMatrix

  sigma2_C_t = sum(w_S_p)
  sigma2_C_Delta = sum(w_S_Delta)
  E_rho2 = sigma2_C_t/(sigma2_C_t+sigma2_C_Delta)
  SEM = sqrt(sigma2_C_Delta)

  return(list("reliability"=E_rho2, "SEM"=SEM, "weights"=weights))
}
