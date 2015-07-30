rankall <- function(outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses="character")

        ## Check that state and outcome are valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
                stop("Invalid outcome")

        ## For each state, find the hospital of the given rank
        if(outcome == "heart attack") {
                idxcol <- 11
        } else if(outcome == "heart failure") {
                idxcol <- 17
        } else if(outcome == "pneumonia") {
                idxcol <- 23
        }


        dat[, idxcol] <- as.numeric(gsub("Not Available", "", dat[, idxcol]))
        dat <- na.omit(dat[, c(2, idxcol, 7)])

        rankhospital <- function(state) {
                dat <- dat[dat[, 3] == state, ]
                nhos <- nrow(dat)

                switch(num, best = {
                        num <- 1
                }, worst = {
                        num <- nhos
                })
        
                if(num > nhos)
                      name <- NA

                p = order(dat[, 2], dat[, 1])
                c(name <- dat[p, ][num, 1], state)
        }

        states <- unique(dat[, 3])
        output <- do.call(rbind, lapply(states, rankhospital))
        output <- output[order(output[, 2]), ]
        rownames(output) <- output[, 2]
        colnames(output) <- c("hospital", "state")

        data.frame(output)

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
