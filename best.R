    ## Return hospital name in that state with lowest 30-day death
    ## rate
best <- function(state, outcome) {
    
    df <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    outcomedata <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% df$State)) { 
        stop("invalid state")
    } else if (!(outcome %in% outcomedata)) {
        stop("invalid outcome")
    } else { 
        dfsplit <- split(df, df$State) 
        dfstate <- dfsplit[[state]] 
        
        if (outcome == "heart attack") {
            dfha <- as.numeric(as.character(dfstate[, 11])) 
            notna <- !is.na(dfha)
            dfha <- dfha[notna]
            min <- min(dfha)
            # return a logical vector about which rows have the min
            rowvec <- grepl(min, dfstate[, 11]) 

        } else if (outcome == "heart failure") { 
            dfhf <- as.numeric(as.character(dfstate[, 17])) 
            # figure out which ones are not NA
            notna <- !is.na(dfhf) 
            dfhf <- dfhf[notna]
            min <- min(dfhf)
            rowvec <- grepl(min, dfstate[, 17])
        } else if (outcome == "pneumonia") {
            dfp <- as.numeric(as.character(dfstate[, 23]))
            notna <- !is.na(dfp)
            dfp <- dfp[notna]
            min <- min(dfp)
            rowvec <- grepl(min, dfstate[, 23])
        }
        # casting as character again
        allnames <- as.character(dfstate$Hospital.Name) 
        minnames <- allnames[rowvec]
        # return the one first in alphabetical order
        minname <- min(minnames) 
        minname
    }
    
    

}
