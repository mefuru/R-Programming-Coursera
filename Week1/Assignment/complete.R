## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
directory <- '/Users/mehulmandania/Dropbox/sandbox/RProgramming/Week1/specdata'
complete <- function(directory, id = 1:332) {
  completeDF <- data.frame()
  for(i in id){
    filename <- sprintf("%03d", i) # add leading zeros if reqd
    filepath <- paste(directory, "/", filename, ".csv", sep="")
    df <- read.csv(filepath)
    df <- df[complete.cases(df),] # Remove non complete cases
    completeDF <- rbind(completeDF, c(i, nrow(df))) # Add vector that contains id and num rows to DF
  }
  colnames(completeDF) <- c('id', 'nobs') # Add column names
  return(completeDF) 
}
