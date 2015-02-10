    ## Return hospital name in that state with lowest 30-day death
    ## rate

rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    outcomedata <- c("heart attack", "heart failure", "pneumonia")
    dfoutput <- data.frame("hospital" = character(54), "state" = character(54), stringsAsFactors = FALSE)
    if (!(outcome %in% outcomedata)) {
        stop("invalid outcome")
    } else { 
        dfsplit <- split(df, df$State) 
        i <- 1 
        for (dfstate in dfsplit) {
            dfstate <- as.data.frame(dfstate) 
            if (outcome == "heart attack") {
                dfha <- as.numeric(as.character(dfstate[, 11])) 
                dfha <- as.factor(dfha)
                notna <- !is.na(dfha)
                dfname <- as.character(dfstate[, 2])[notna]
                dfrate <- as.numeric(as.character(dfstate[, 11]))[notna]
                
            } else if (outcome == "heart failure") { 
                dfhf <- as.numeric(as.character(dfstate[, 17])) 
                dfhf <- as.factor(dfhf) 
                notna <- !is.na(dfhf) 
                dfname <- as.character(dfstate[, 2])[notna]
                dfrate <- as.numeric(as.character(dfstate[, 17]))[notna]
            } else if (outcome == "pneumonia") {
                dfp <- as.numeric(as.character(dfstate[, 23]))
                dfp <- as.vector(dfp)
                # figure out which ones are not NA
                notna <- !is.na(dfp)
                dfname <- as.character(dfstate[, 2])[notna]
                dfrate <- as.numeric(as.character(dfstate[, 23]))[notna]
            }
            dfslim <- data.frame(dfname = dfname, dfrate = dfrate, stringsAsFactors = FALSE)
            ordered <- dfslim[order(as.numeric(as.character(dfrate)), as.character(dfname)), ] 
            #namevec <- as.vector(ordered$dfname)
            if (num == "worst") {
                num1 <- length(ordered$dfname)
            }
            else if (num == "best") {
                num1 <- 1
            } else {
                num1 <- num
            }
            namelist <- as.vector(ordered$dfname)
            minname <- namelist[num1]
            
            dfoutput$hospital[i] <- as.character(minname) 
            dfoutput$state[i]    <- as.character(dfstate$State)[1] 
            i <- i + 1
        }
    }
    
    dfoutput

}
