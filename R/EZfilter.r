#' EZ Filter
#' 
#' Identifies coefficient of variance or degree of variability among samples for a given gene in a large data set and adds a new column for the coefficient of variance to the existing data frame. The "COVAR" column indicates the number of samples with coefficient of variance greater than a given criteria. The default criteria is set at 0.5. For example, if coefficient of variance for samples 1-5 for gene 1 is greater than 50%, the "COVAR" column reads 5 meaning all 5 samples have coefficient of variance of greater than 50% for gene 1.    
#' @param dataFrame name of data frame 
#' @param label unique column name to assign to coefficient of variance
#' @param covar coefficient of variance with default value 0.5
#' @keywords filter, coefficient, variance, EZ
#' @export 
#' @examples 
#' df <- EZfilter(df, "SAMPLE1")

EZfilter <- function(dataFrame, label, covar = 0.5) {
  #creating a column and repopulating it with zeroes 
  if(!("COVAR" %in% colnames(dataFrame)))
  {
    dataFrame["COVAR"] = 0
  }
  meanLabel <- paste("MEAN", label, sep="_")
  stdLabel <- paste("STD", label, sep="_")
  for (i in 1:nrow(dataFrame)){
    #std/mean gives coefficient of variance
    if (dataFrame[[stdLabel]][i]/dataFrame[[meanLabel]][i] > covar){
      #if covar is greater than 0.5 add 1 to COVAR column
      dataFrame[["COVAR"]][i] = dataFrame[["COVAR"]][i] + 1 
    }
    
  }
  return(dataFrame)
}

