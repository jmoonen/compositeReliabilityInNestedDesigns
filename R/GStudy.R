#' GStudy for a dataset in which every student p has a potentially differing number of scores i on each assessment type m.
#' i.e. model i: (p x m). The output gives descriptive statistics, reliability coefficient and SEM for each assessment type.
#'
#' @param mydata A dataframe containing columns ID, Type, Score (numeric)
#' @param nrDigitsOutput Integer, number of digits in the output
#'
#' @return Matrix with descriptive statistics for each Type of assessment
#' @export
#'
#' @examples
#' GStudy(mydata,nrDigitsOutput=4)

GStudy <- function(mydata, nrDigitsOutput=4){
  nrAssessmentsPerID <- checkDatasets(mydata)

  Type = Freq = NULL
  types <- sort(unique(mydata$Type))

  # Initialize variables
  statisticMatrix <- matrix(0, nrow=11, ncol=length(types),
                            dimnames=list(c("NrAssessments", "NrAssessees", "MeanScore", "StDev", "AvgNrAssessments", "HarmonicMean", "MaxNrAssessments", "MinNrAssessments", "MedianNrAssessments", "ReliabilityCoeff_HM", "SEM_HM"), types))

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

  statisticMatrix <- round(statisticMatrix,nrDigitsOutput)
  return (statisticMatrix)
}
