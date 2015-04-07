source('formatFilename.R')

complete <- function(directory, id = 1:332) {
  # returns a data frame w/ id and # observations
  dfComplete <- data.frame(id = numeric(), nobs = numeric())
  for (fileId in id) {
    filename <- formatFilename(fileId)
    filepath <- paste(directory, '/', filename, sep='')
    df <- read.csv(filepath)
    dfComplete <- rbind(dfComplete, data.frame(id = fileId, nobs = nrow(df[complete.cases(df), ])))
  }
  dfComplete
}