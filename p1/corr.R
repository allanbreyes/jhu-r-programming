source('complete.R')
source('formatFilename.R')

corr <- function(directory, threshold = 0) {
  # returns a numeric vector of correlations
  
  observations <- complete(directory)
  validIds <- observations$id[observations$nobs > threshold]
  result <- numeric()
  sulfate <- numeric()
  nitrate <- numeric()
  
  for (id in validIds) {
    filename <- formatFilename(id)
    filepath <- paste(directory, '/', filename, sep='')
    df <- read.csv(filepath)
    result <- c(result, cor(df$sulfate, df$nitrate, use='complete.obs'))
  }
  
  result
}