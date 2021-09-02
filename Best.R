outcome <- read.csv("outcome-of-care-measures.csv")#, colClasses = "character")
head(outcome)
str(outcome)
names(outcome)
ncol(outcome) # number of columns in the dataset
nrow(outcome)
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
class(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
hist(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, xlab = "Hospital 30 Day Death Mortality Rates 
     from Heart Attack", main = "Hospital 30 Day Death Mortality Rates from Heart Attack" )






best <- function(state, outcome) {
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header = T)
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% unique(outcome_data$State))) {
    stop ("invalid state")
  }
  else if (!outcome %in% possibleOutcomes) {
    stop ("invalid outcome")
  } 
  else {
    new_dataframe <- data.frame(outcome_data$Hospital.Name,
                                outcome_data$State,
                                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    cnames <- c("Hospital", "State", possibleOutcomes)
    colnames(new_dataframe) <-  cnames
    state_data <- new_dataframe[which(new_dataframe[, 'State'] == state), ] #  extracting data for specific state
    state_data[, outcome] <- as.numeric(state_data[, outcome])
    #state_data[, outcome]
    min_value <- min(state_data[, outcome], na.rm = T) 
    hospital_min <- state_data[which(state_data[, outcome] == min_value), ]$Hospital
    output <- hospital_min[order(hospital_min)]
    return (output[1])
    
  }
}

best("NY","pneumonia")
best("MD", "heart attack")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

