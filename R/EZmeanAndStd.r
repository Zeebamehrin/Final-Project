#' EZ Mean & Standard Deviation  
#' 
#' Computes mean and standard deviation for each sample set in large data set and adds a new column for mean and std calculated for each sample set to the existing data frame. This functions requires tidyverse and matrixstats packages.  
#' @param dataFrame name of data frame
#' @param columns a vector of columns to calculate mean and standard deviation
#' @param label unique column name to assign to the mean and standard deviation column 
#' @keywords mean, std, EZ
#' @export 
#' @examples 
#' df <- EZmeanAndstd(df, c('SAMPLE.1', 'SAMPLE.1.1', 'SAMPLE.1.2'), "SAMPLE1")

EZmeanAndstd <- function(dataFrame, columns, label)
{
  meanLabel <- paste("MEAN", label , sep="_")
  stdLabel <- paste("STD", label , sep="_")
  dataFrame <- dataFrame %>% 
    mutate(!!meanLabel := rowMeans(.[columns]), !!stdLabel := rowSds(as.matrix(.[columns])))
  return(dataFrame)
}
