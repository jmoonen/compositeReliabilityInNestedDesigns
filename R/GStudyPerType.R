GStudyPerType <- function(dataPerAssessmentType) {
  myformula = "Score ~ 1 + (1|ID)"
  fit <- lmer(formula=myformula, data=dataPerAssessmentType)
  summary(fit)
  sum <- summary(fit)
  ngrps <- as.data.frame(sum$ngrps)
  ngrps$grp <- row.names(ngrps)
  colnames(ngrps) <- c("n","grp")
  varcor <- as.data.frame(VarCorr(fit)) %>% left_join(ngrps, by=c("grp"))
  totvar <- sum(varcor$vcov)
  varcor$perc <- 100*varcor$vcov/totvar
  res <- varcor %>% select(-var1, -var2, -sdcor);
  rownames(res) <- res$grp
  return(res)
}
