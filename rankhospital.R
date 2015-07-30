rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses="character")

        ## Check that state and outcome are valid
        if(!state %in% unique(dat$State))
                stop("Invalid state")

        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop("Invalid outcome")

        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(outcome == "heart attack") {
                idxcol <- 11
        } else if(outcome == "heart failure") {
                idxcol <- 17
        } else if(outcome == "pneumonia") {
                idxcol <- 23
        }


        dat[, idxcol] <- as.numeric(gsub("Not Available", "", dat[, idxcol]))
        dat <- na.omit(dat[dat$State == state, c(2, idxcol)])
        nhos <- nrow(dat)

        switch(num, best = {
                num <- 1
        }, worst = {
                num <- nhos
        })

        if(num > nhos)
              return(NA)

        p = order(dat[, 2], dat[, 1])
        dat[p, ][num, 1]
}
