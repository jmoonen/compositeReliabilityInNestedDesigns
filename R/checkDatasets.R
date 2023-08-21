#' checkDatasets: assert that the given datasets adhere to the assumptions and requirements of this package
#' i.e. the data set 'mydata' is a dataframe with 3 columns, named "ID", "Type" and "Score", column "Score" contains numeric data,
#' and each combination of "ID" and "Type" exists at least once, data set n contains a numerical value for each "Type",
#' and data set weights contains a numerical value for each "Type" and the sum of all values is equal to 1.
#'
#' @param mydata A dataframe containing columns ID, Type, Score (numeric)
#' @param n A vector containing for each Type the number of score or assessments assessments, e.g. averages, requirements.
#' @param weights A vector containing for each Type the weight assigned to it. The sum of weights should be equal to 1.
#'
#' @return A list with the number of Assessments per ID per Type
#' @export
#'
#' @examples
#' checkDatasets(mydata, n=c("A"=10, "B"=5, "C"=2), weights=c("A"=1/3,"B"=1/3, "C"=1/3))

checkDatasets <- function(mydata, n = NULL, weights = NULL){
  Type = Freq = NULL

  # check that the dataset if a dataframe with 3 columns
  stopifnot(is.data.frame(mydata))
  stopifnot(ncol(mydata) == 3)

  # check that the rownames are "ID", "Type" and "Score"
  stopifnot(colnames(mydata) == c("ID","Type","Score"))

  # check that Score is numeric
  stopifnot(is.numeric(mydata$Score))

  # assert that each assessee has at least one score for each Type
  countMatrix <- plyr::ddply(mydata, plyr::.(mydata$ID, mydata$Type), nrow)
  names(countMatrix) <- c("ID", "Type", "Freq")
  nrAssessmentsPerID <- tidyr::spread(countMatrix, Type, Freq)
  stopifnot(is.na(nrAssessmentsPerID) == FALSE)

  if(!is.null(n)) {
    # Each type is included in n
    types <- sort(unique(mydata$Type))
    stopifnot(sort(names(n)) == types)

    # The values in n are numeric
    if(!is.numeric(n)) {
      n <- as.numeric(n)
      stopifnot(is.na(n) == FALSE)
    }
  }
  if(!is.null(weights)) {
    # Each type is included in weights
    types <- sort(unique(mydata$Type))
    stopifnot(sort(names(weights)) == types)

    # The values in weights are numeric
    if(!is.numeric(weights)) {
      weights <- as.numeric(weights)
      stopifnot(is.na(weights) == FALSE)
    }
    # The sum of values is equal to 1
    epsilon = 0.0001
    stopifnot(abs(sum(weights))-1<epsilon)
  }

  return(nrAssessmentsPerID)
}
