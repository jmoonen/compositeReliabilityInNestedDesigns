#' GStudyPerType: This function is mainly used within calculateVarCov.R, but can be executed on its own to determine the reliability coefficient and SEM for a dataset with a single type of assessment.
#'
#' @param dataPerAssessmentType A dataframe containing columns ID, Type, Score (numeric), with only one value in column Type
#'
#' @return A matrix presenting the observerd varianced and residual, number of ID's and the percentage of the total variance for each group
#' @export

GStudyPerType <- function(dataPerAssessmentType) {
  checkDatasets(dataPerAssessmentType)
  myformula = "Score ~ 1 + (1|ID)"
  fit <- lme4::lmer(formula=myformula, data=dataPerAssessmentType)
  summary(fit)
  sum <- summary(fit)
  ngrps <- as.data.frame(sum$ngrps)
  ngrps$grp <- row.names(ngrps)
  colnames(ngrps) <- c("n","grp")

  var1 = var2 = sdcor = NULL
  varcor <- as.data.frame(lme4::VarCorr(fit)) %>% dplyr::left_join(ngrps, by=c("grp"))
  totvar <- sum(varcor$vcov)
  varcor$perc <- 100*varcor$vcov/totvar
  res <- varcor %>% dplyr::select(-var1, -var2, -sdcor);
  rownames(res) <- res$grp

  return(res)
}
