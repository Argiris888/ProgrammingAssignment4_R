rankhospital <- function(state, outcome, num = "best") {
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% unique(outcome_data$State))) {
    stop ("invalid state")
  }
  else if (!outcome %in% possibleOutcomes) {
    stop ("invalid outcome")
  } 
  
  new_dataframe <- data.frame(outcome_data[, "Hospital.Name"],
                              outcome_data[, "State"],
                              outcome_data[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"],
                              outcome_data[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"],
                              outcome_data[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
  colNames <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  names(new_dataframe) <- colNames
  state_dataframe <- new_dataframe[which(new_dataframe[ , 'State'] == state), ]
  
  
  state_dataframe[, outcome] <- as.numeric(state_dataframe[, outcome]) 
  case_vector <- state_dataframe[, outcome]
  #case_dataframe <- state_dataframe[order(case_vector), ]
  case_dataframe <- state_dataframe[!is.na(case_vector), ]
  ordered_dataframe <- case_dataframe[order(case_dataframe[, outcome], case_dataframe[, "Hospital"]), ] ## 2 levels of ordering!!!
  if (!is.numeric(num)) {
    if (num == "best") {
      num <- 1
      output <- ordered_dataframe[num, "Hospital"]
      return (output)
    }
    else if (num == "worst") {
      num <- nrow(ordered_dataframe)
      output <- ordered_dataframe[num, "Hospital"]
      return (output)
    }
    else {
      stop ("invalid num")
    }
  }  
  else {
    if (num > nrow(state_dataframe)) {  
      return (NA)
    }
    else {
      output <- ordered_dataframe[num, "Hospital"]
      return (output)
    }
  }
}

#debug(rankhospital)
rankhospital("AZ", "heart attack", 10)
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
