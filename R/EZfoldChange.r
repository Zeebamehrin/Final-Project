#' EZ Fold Change 
#' 
#' Computes log2 fold change for each sample in a large data set and adds a new column for the calculated value for each sample to the existing data frame.    
#' @param dataFrame name of data frame 
#' @param label unique column name previously provided to EZmeanAndstd function
#' @keywords foldchange, fold change, EZ
#' @export 
#' @examples 
#' df <- EZfoldChange(df, "SAMPLE1")

EZfoldChange <- function(dataFrame, label)
{
  meanLabel <- paste("MEAN", label , sep="_")
  log2Label <- paste("LOG2", label , sep="_")
  dataFrame[log2Label] = sapply(dataFrame[meanLabel], log2)
  return(dataFrame)
}