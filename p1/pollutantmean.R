source('formatFilename.R')

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # returns the mean of the pollutant across all monitors list in the 'id'
  # vector, ignoring NA values.
  
  dataset <- numeric()
  
  for (fileId in id) {
    filename <- formatFilename(fileId)
    filepath <- paste(directory, '/', filename, sep='')
    df <- read.csv(filepath)
    dataset <- c(dataset, df[, pollutant])
  }
  
  mean(dataset, na.rm=TRUE)
}