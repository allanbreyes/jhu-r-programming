formatFilename <- function(id) {
  # returns a text-friendly filename e.g. 019.csv
  
  if (id < 10) {
    filename <- paste('00', id, '.csv', sep='')
  } else if (id < 100) {
    filename <- paste('0', id, '.csv', sep='')
  } else {
    filename <- paste(id, '.csv', sep='')
  }
  filename
}

# unit test
stopifnot(formatFilename(5) == '005.csv')
stopifnot(formatFilename(50) == '050.csv')
stopifnot(formatFilename(500) == '500.csv')
