#' DStudy: the program presents the reliability and the SEM for different numbers of assessments per type.
#' Both the reliability and the SEM are presented in graphs for differing numbers of assessments,
#' given insight in the impact on the reliability if more or less assessments per type were required or advised.
#'
#' @param mydata A dataset containing columns ID (int), Type (enum), Score (float)
#' @param maxNrAssessments The maximum number of assessments per type on with the D study is executed
#'
#' @return A list containing 2 plots: reliability (plotRel) and Standard Error of Measurement SEM (plotSEM)
#' @export
#'
#' @examples
#' plots <- DStudy(mydata, 60)
#' plots$plotRel
#' plots$plotSEM

DStudy <- function(mydata, maxNrAssessments = 60){
  nr = value = variable = NULL

  types <- sort(unique(mydata$Type))

  Dstudy_E_rho2 <- matrix(0, nrow=maxNrAssessments, ncol=length(types), dimnames=list(1:maxNrAssessments, types))
  Dstudy_SEM <- matrix(0, nrow=maxNrAssessments, ncol=length(types), dimnames=list(1:maxNrAssessments, types))

  for(i in 1:maxNrAssessments) {
    n <- rep(i, times=length(types))
    names(n) = types

    #source("includes/CalculateReliability.R", print.eval = TRUE)
    outputRel <- calculateReliability(types, mydata, n) #calculateReliability(types,varCovMatrix,n,w,optimizeSEM,FALSE)

    Dstudy_E_rho2[i,] = outputRel$E_rho2_vector
    Dstudy_SEM[i,] = outputRel$SEM_vector
  }

  D <- as.data.frame(Dstudy_E_rho2)
  D$nr <- c(1:maxNrAssessments)

  df <- reshape2::melt(D,  id.vars = "nr", measure.vars = types)
  # plot reliabilities on same grid, each series colored differently
  plotRel <- ggplot2::ggplot(df, ggplot2::aes(nr,value)) + ggplot2::geom_line(ggplot2::aes(colour = variable)) + ggplot2::scale_x_continuous(breaks=seq(0,maxNrAssessments,5)) + ggplot2::scale_y_continuous(breaks=seq(0,1,0.1)) + ggplot2::geom_hline(yintercept=0.8, linetype="dashed", color = "black")

  S <- as.data.frame(Dstudy_SEM)
  S$nr <- c(1:maxNrAssessments)

  dfS <- reshape2::melt(S,  id.vars = "nr", measure.vars = types)
  # plot SEMs on same grid, each series colored differently
  plotSEM <- ggplot2::ggplot(dfS, ggplot2::aes(nr,value)) + ggplot2::geom_line(ggplot2::aes(color = variable)) + ggplot2::scale_x_continuous(breaks=seq(0,maxNrAssessments,5)) + ggplot2::scale_y_continuous(breaks=seq(0,max(dfS$value),0.1)) + ggplot2::geom_hline(yintercept=0.26, linetype="dashed", color = "black")

  return(list("plotRel"=plotRel, "plotSEM"=plotSEM))
}
