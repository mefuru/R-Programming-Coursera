rankhospital <- function(state, outcome, num) {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")) {
        stop("invalid outcome")
    }
    states <- unique(outcomeData$State)
    if(!state %in% states) stop("invalid state")
    ## Return hospital name in that state with lowest 30-day death
    ## Set col index for outcome
    if (outcome == "heart attack") outcomeCol <- 11
    if (outcome == "heart failure") outcomeCol <- 17
    if (outcome == "pneumonia") outcomeCol <- 23
    ## Filter DF on state
    outcomeData <- outcomeData[outcomeData$State==state,]
    ## Convert using as.numeric, supressing coercian warnings
    suppressWarnings(outcomeData[, outcomeCol] <- as.numeric(outcomeData[, outcomeCol]))
    if (class(num) == "numeric") {
        ## Order DF on outcomeCol, and then name
        outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[num,2])
    }
    if (num=="best") {
        ## Order DF on outcomeCol, and then name
        outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[1,2])
    }
    if (num=="worst") {
        ## Order DF on reverse of outcomeCol, and then name
        outcomeData <- outcomeData[order(-outcomeData[,outcomeCol], outcomeData[,2]), ]
        return (outcomeData[1,2])
    }
}

## TESTS
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)