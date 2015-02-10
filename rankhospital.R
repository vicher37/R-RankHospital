    ## Return hospital name in that state with lowest 30-day death
    ## rate

rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    outcomedata <- c("heart attack", "heart failure", "pneumonia")
    
    # test if this state is in state vector using %in%
    if (!(state %in% df$State)) { 
        stop("invalid state")
    } else if (!(outcome %in% outcomedata)) {
        stop("invalid outcome")
    } else { 
        dfsplit <- split(df, df$State) 
        dfstate <- dfsplit[[state]] 
        
        if (outcome == "heart attack") {
            dfstate[, 11] <- as.numeric(as.character(dfstate[, 11])) 
            notna <- !is.na(dfstate[, 11])
            dfname <- as.character(dfstate[, 2])[notna]
            dfrate <- dfstate[, 11][notna]
            
        } else if (outcome == "heart failure") { 
            dfhf <- as.numeric(as.character(dfstate[, 17])) 
            # figure out which ones are not NA
            notna <- !is.na(dfstate[, 17]) 
            dfname <- as.character(dfstate[, 2])[notna]
            dfrate <- dfstate[, 17][notna]
        } else if (outcome == "pneumonia") {
            dfp <- as.numeric(as.character(dfstate[, 23]))
            notna <- !is.na(dfstate[, 23]) 
            dfname <- as.character(dfstate[, 2])[notna]
            dfrate <- dfstate[, 23][notna]

        }
        dfslim <- data.frame(dfname = dfname, dfrate = dfrate)
        ordered <- dfslim[order(as.numeric(as.character(dfrate)), as.character(dfname)), ] 
        
        if (num == "worst") {
            num <- length(dfslim$dfname)
        }
        if (num == "best") {
            num <- 1
        }
        
        minname <- as.character(ordered$dfname)[num]
        minname
    }
    
    

}
