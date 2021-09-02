rankall <- function(outcome, num = "best") {
  
  # ##check session
  # outcome <- "heart attack"
  # num <- "best"
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", header = T )
  
  ## Check that state and outcome are valid
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if ( ! (outcome %in% possibleOutcomes)) {
    stop ("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  ndf <- data.frame(df$Hospital.Name, df$State, df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                    df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                    df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  colNames <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  names(ndf) <- colNames
  ndf[, outcome] <- as.numeric(ndf[, outcome])
  #ndf <- ndf[!is.na(ndf[, outcome]), ]
  list_ndf <- split.data.frame(ndf, ndf$State) #by state
  cdf <- data.frame(matrix(ncol = 2, nrow = 0))
  names(cdf) <- c("hospital", "state")
  #cdf <- setNames(matrix(ncol = 2, nrow = 0), c("hospital", "state"))
  
  for (state in list_ndf) {
    state_df <- state[!is.na(state[, outcome]), ]
    state_ord <- state_df[order(state_df[, outcome], state_df[, "Hospital"]), ]
    if (is.numeric(num)) {
      row_output <- data.frame(state_ord[num, "Hospital"], state[1, "State"])
      names(row_output) <- c("hospital", "state")
      cdf <- rbind(cdf, row_output)
    }
    else {
      if (num == "best") {
        num2 <- 1
        row_output <- data.frame(state_ord[num2, "Hospital"], state[1, "State"])
        names(row_output) <- c("hospital", "state")
        cdf <- rbind(cdf, row_output)
      }
      else if (num == "worst") {
        num2 <- nrow(state_ord)
        row_output <- data.frame(state_ord[num2, "Hospital"], state[1, "State"])
        names(row_output) <- c("hospital", "state")
        cdf <- rbind(cdf, row_output)
      }
      else {
        stop("invalid num")
      }
      
    }
    
  }
  
  return (cdf)
  #names(state)
  #state_ord
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}

tail(rankall("heart failure"), 10)
#debug(rankall)
tail(rankall("pneumonia", "worst"), 3)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
