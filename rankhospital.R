rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with given rank 30-day death
                
        outcomeTypes <- c("heart attack", "heart failure", "pneumonia")
        data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        if (!is.element(state, data[,7])) stop('invalid state')
        if (!is.element(outcome, outcomeTypes)) stop('invalid outcome')
        
        data <- data[,c(2,7,11,17,23)]
                
        for (i in 3:5) data[,i] <- as.numeric(data[,i])
        
        colnames(data) <- c("Hospital.Name","State", "heart attack", "heart failure", "pneumonia")
        data <- subset(data, State == state)
        data <- data[order(data[outcome], data['Hospital.Name']),]
        
        countRanks <- nrow(data[!is.na(data[outcome]),])
        #print(countRanks)
        
        if (num == "best") {
                num <- 1
        }
        else  if (num == "worst") {
                num <- countRanks
        }
        
        data[num,1]
        
}
