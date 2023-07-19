#' GStudy for a dataset in which every student p has a potentially differing number of scores i on each assessment type m.
#' i.e. model i: (p x m)
#'
#' @param mydata A dataset containing columns ID (int), Type (enum), Score (float)
#' @param nrDigitsOutput Integer, number of digits in the output
#' @param optimizeSEM  Boolean, if TRUE, the weights are adjusted in order to minimize the Standard Error of Measurement (SEM)
#'
#' @return List containing a matrix with descriptive statistics (statisticMatrix) for each Type, and textual output (outputGstudy) of the composite reliabitiy
#' @export
#'
#' @examples
#' outputG <- GStudy(mydata,nrDigitsOutput=4,optimizeSEM=TRUE)
#' outputG$statisticMatrix
#' outputG$outputGstudy

GStudy <- function(mydata, nrDigitsOutput=4, optimizeSEM=TRUE){
  Type = Freq = NULL
  types <- sort(unique(mydata$Type))

  # Initialize variables
  statisticMatrix <- matrix(0, nrow=11, ncol=length(types),
                            dimnames=list(c("NrAssessments", "NrAssessees", "MeanScore", "StDev", "AvgNrAssessments", "HarmonicMean", "MaxNrAssessments", "MinNrAssessments", "MedianNrAssessments", "ReliabilityCoeff_HM", "SEM_HM"), types))

  countMatrix <- plyr::ddply(mydata, plyr::.(mydata$ID, mydata$Type), nrow)
  names(countMatrix) <- c("ID", "Type", "Freq")
  nrAssessmentsPerID <- tidyr::spread(countMatrix, Type, Freq)

  for(aType in types) {
    dataPerType <- dplyr::filter(mydata,Type==aType)

    statisticMatrix["NrAssessments",aType] = sum(nrAssessmentsPerID[,aType])
    statisticMatrix["NrAssessees",aType] = length(unique(dataPerType$ID))
    statisticMatrix["MeanScore",aType] = mean(dataPerType$Score)
    statisticMatrix["StDev",aType] = stats::sd(dataPerType$Score)
    statisticMatrix["HarmonicMean",aType] = psych::harmonic.mean(nrAssessmentsPerID[,aType])
    statisticMatrix["AvgNrAssessments",aType] = statisticMatrix["NrAssessments",aType]/statisticMatrix["NrAssessees",aType]
    statisticMatrix["MaxNrAssessments",aType] = max(nrAssessmentsPerID[,aType])
    statisticMatrix["MinNrAssessments",aType] = min(nrAssessmentsPerID[,aType])
    statisticMatrix["MedianNrAssessments",aType] = stats::median(nrAssessmentsPerID[,aType])
  }

  n <- statisticMatrix["HarmonicMean",]
  reliability <- calculateReliability(mydata, n)

  statisticMatrix["ReliabilityCoeff_HM",] = reliability$E_rho2_vector[,"value"]
  statisticMatrix["SEM_HM",] = reliability$SEM_vector[,"value"]

  #equal weights to start with for composite reliability
  w <- rep(1/length(types), times=length(types))
  names(w) = types

  compRel <- computeCompositeReliability(mydata, statisticMatrix["HarmonicMean",], w, TRUE)

  outputGstudy <- data.frame(c("*** Composite reliability with harmonic means per assessment type m ***","Composite reliability = ","Composite SEM = "),
                             c("", round(compRel$reliability,nrDigitsOutput), round(compRel$SEM,nrDigitsOutput)))
  colnames(outputGstudy) <- c("","")

  compRel <- computeCompositeReliability(mydata, statisticMatrix["AvgNrAssessments",], w, FALSE)

  outputGstudy <- rbind(outputGstudy, c("*** Composite reliability with average number per assessment type m ***",""))
  outputGstudy <- rbind(outputGstudy, c("Composite reliability = ",round(compRel$reliability,nrDigitsOutput)))
  outputGstudy <- rbind(outputGstudy, c("Composite SEM = ",round(compRel$SEM,nrDigitsOutput)))


  if(optimizeSEM) {
    optSemCompRel <- computeCompositeReliability(mydata, statisticMatrix["AvgNrAssessments",], w, TRUE)
    outputGstudy <- rbind(outputGstudy, c("*** Composite Reliability with optimized weights for SEM***",""))
    outputGstudy <- rbind(outputGstudy, c("Composite reliability = ",round(optSemCompRel$reliability,nrDigitsOutput)))
    outputGstudy <- rbind(outputGstudy, c("Composite SEM = ",round(optSemCompRel$SEM,nrDigitsOutput)))
    outputGstudy <- rbind(outputGstudy, c(paste("Weights (",toString(names(optSemCompRel$weights)), ") = ", sep=""),paste("(",toString(round(optSemCompRel$weights,nrDigitsOutput)),")",sep="")))
  }

  statisticMatrix <- round(statisticMatrix,nrDigitsOutput)
  return (list("statisticMatrix" = statisticMatrix, "outputGstudy" = outputGstudy))
}
