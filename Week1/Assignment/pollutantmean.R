## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

directory <- '/Users/mehulmandania/Dropbox/sandbox/RProgramming/Week1/specdata'
pollutantmean <- function(directory, pollutant, id = 1:332) {
  totalPollutants <- numeric() ## Vector to collate pollutant data
  for (i in id) {
    filename <- sprintf("%03d", i) ## add leading zeros if reqd
    filepath <- paste(directory, "/", filename, ".csv", sep="")
    df <- read.csv(filepath)
    pollutant_data <- na.omit(df[pollutant])
    if(pollutant=='sulfate'){
      totalPollutants <- c(totalPollutants, pollutant_data$sulfate)
    } else {
      totalPollutants <- c(totalPollutants, pollutant_data$nitrate)
    }
  }
  round(mean(totalPollutants), 3)
}
## Test cases
pollutantmean(directory, 'sulfate', 1:10)
pollutantmean(directory, "nitrate", 70:72)
pollutantmean(directory, "nitrate", 23)