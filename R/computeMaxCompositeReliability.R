#' computeMaxCompositeReliability: multivariate generalizability theory approach to estimate the maximum composite reliability of student performance across different types of assessments.
#'
#' @param mydata A dataframe containing columns ID, Type, Score (numeric)
#' @param n A vector containing for each Type the number of score or assessments assessments, e.g. averages, requirements.
#'
#' @return A list containing the composite reliability coefficient, the SEM and the distribution of weights.
#' @export
#'
#' @examples
#' \donttest{
#' compMaxRel <- computeMaxCompositeReliability(mydata, n=c("A"=3, "B"=2, "C"=1))
#' compMaxRel$reliability
#' compMaxRel$SEM
#' compMaxRel$weights
#' }

computeMaxCompositeReliability <- function(mydata, n) {
  checkDatasets(mydata, n, weights=NULL)

  f.CRM <- function(w) {
    varcov <- calculateVarCov(mydata, n)
    weightMatrix <- w %*% t(w)

    w_S_p <- varcov$S_p * weightMatrix
    w_S_delta <- varcov$S_delta * weightMatrix

    sigma2_C_t = sum(w_S_p)
    sigma2_C_Delta = sum(w_S_delta)
    #used as minimization function, thus objective * -1
    f.w <- -1 *sigma2_C_t/(sigma2_C_t+sigma2_C_Delta)
  }

  equal <- function(w) {
    return(sum(w))
  }

  compRelMax <- Rsolnp::solnp(pars=rep(1/length(n),length(n)), fun=f.CRM, eqfun=equal, eqB = 1, LB=rep(0.,length(n)), UB=rep(1.,length(n)))
  return(computeCompositeReliability(mydata, n=n, weights=compRelMax$pars, FALSE))
}
