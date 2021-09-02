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
  outcome_data <- read.csv("outcome-of-care-measures.csv")
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% unique(outcome_data$State))) {
    stop ("invalid state")
  }
  else if (!outcome %in% possibleOutcomes))) {
    stop ("invalid outcome")
  }  
}
outcome$State
unique(outcome$State)
"NV" %in% unique(outcome$State)
(1 %in% 3)==T
state= "ALa"
if (!(state %in% unique(outcome$State))) {
  print ("ok")
}
stop("invalid state")


state <- "AL"
outcome
outcome_data <- read.csv("outcome-of-care-measures.csv")
possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")

"heart attack" %in% c("heart attack", "heart failure", "pneumonia")
