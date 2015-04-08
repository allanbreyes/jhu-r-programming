dataDirectory <- 'data/'
outcomesCsv <- 'outcome-of-care-measures.csv'
readCsv <- function(filename) {
  read.csv(paste(dataDirectory, filename, sep=''), stringsAsFactors=FALSE)
}

mapOutcome <- function(outcome) {
  table <- new.env()
  table[['heart attack']] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  table[['heart failure']] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  table[['pneumonia']] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  table[[outcome]]
}

best <- function(state, outcome) {
  # read outcome data
  df = readCsv(outcomesCsv)
  
  # check state
  validStates <- unique(df$State)
  if (!(state %in% validStates)) {
    stop('invalid state')
  }
  
  # map and check outcome
  outcomeCol <- mapOutcome(outcome)
  if (is.null(outcomeCol)) {
    stop('invalid outcome')
  }
  
  # filter by state
  df <- subset(df, State==state)
  
  # sort by outcome and alphabetically
  df[, outcomeCol] <- suppressWarnings(as.numeric(df[, outcomeCol]))
  df <- df[order(df[outcomeCol], df['Hospital.Name']), ]
  hospitals <- as.character(df['Hospital.Name'][!is.na(df[outcomeCol])])
  head(hospitals, 1)
}

# unit tests
stopifnot(best('TX', 'heart attack')=="CYPRESS FAIRBANKS MEDICAL CENTER")
stopifnot(best('TX', 'heart failure')=="FORT DUNCAN MEDICAL CENTER")
stopifnot(best('MD', 'heart attack')=="JOHNS HOPKINS HOSPITAL, THE")
stopifnot(best('MD', 'pneumonia')=="GREATER BALTIMORE MEDICAL CENTER")
