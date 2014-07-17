## Return hospital name in that state with lowest 30-day death
rankall <- function(outcome, num="best") {
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")) {
        stop("invalid outcome")
    }
    ## Set col index for outcome
    if (outcome == "heart attack") outcomeCol <- 11
    if (outcome == "heart failure") outcomeCol <- 17
    if (outcome == "pneumonia") outcomeCol <- 23    
    ## For each state, find the hospital of the given rank
    states <- unique(outcomeData$State)
    rankMatrix <- sapply(states, function(state){
        ## Filter DF on state
        outcomeData <- outcomeData[outcomeData$State==state,]
        ## Convert using as.numeric, supressing coercian warnings
        suppressWarnings(outcomeData[, outcomeCol] <- as.numeric(outcomeData[, outcomeCol]))
        if (class(num) == "numeric") { ## Order DF on outcomeCol, and then name
            outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
            return (c(state, outcomeData[num,2]))
        }
        if (num=="best") { ## Order DF on outcomeCol, and then name
            outcomeData <- outcomeData[order(outcomeData[,outcomeCol], outcomeData[,2]), ]
            return (c(state, outcomeData[1,2]))
        }
        if (num=="worst") { ## Order DF on reverse of outcomeCol, and then name
            outcomeData <- outcomeData[order(-outcomeData[,outcomeCol], outcomeData[,2]), ]
            return (c(state, outcomeData[1,2]))
        }
    })
    result <- data.frame(rankMatrix[2,], rankMatrix[1,]) # Insert values into DF
    colnames(result) <- c("hospital", "state")
    rownames(result) <- result$state
    result <- result[order(result$state),]
    return(result)
}

## TESTS
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)