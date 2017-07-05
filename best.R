best <- function(state, outcome) {
  ## Read outcome data
  
  reader <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  if(!is.element(state,reader$State)) {
    stop("invalid state")
  }
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  states <- which(reader$State == state)
  searcher <- reader[states,]
  
  if(outcome == "heart attack"){
    heartAttack <- na.omit(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    smallest <- min(heartAttack)
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == smallest)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    return(hospital)
  }
  if(outcome == "heart failure"){
    heartFail <- na.omit(as.numeric(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    smallest <- min(heartFail)
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == smallest)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    return(hospital)
  }
  if(outcome == "pneumonia"){
    pneumonia <- na.omit(as.numeric(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    smallest <- min(pneumonia)
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == smallest)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    return(hospital)
  }
}

rankhospital <- function(state, outcome, num){
  
  reader <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  if(!is.element(state,reader$State)) {
    stop("invalid state")
  }
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  states <- which(reader$State == state)
  searcher <- reader[states,]
  
  if(outcome == "heart attack"){
    heartAttack <- na.omit(as.numeric(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    if(num=="best"){
      place <- min(heartAttack)
    } else if(num == "worst") {
      place <- max(heartAttack)
    } else {
      place <- heartAttack[order(heartAttack)]
      place <- heartAttack[num]
    }
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == place)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    hospital <- hospital[order(hospital)]
    return(hospital[1])
  }
  if(outcome == "heart failure"){
    heartFail <- na.omit(as.numeric(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    if(num=="best"){
      place <- min(heartFail)
    } else if(num == "worst") {
      place <- max(heartFail)
    } else {
      place <- heartFail[order(heartFail)]
      place <- heartFail[num]
    }
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == place)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    hospital <- hospital[order(hospital)]
    return(hospital[1])
  }
  
  if(outcome == "pneumonia"){
    pneumonia <- na.omit(as.numeric(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    if(num=="best"){
      place <- min(pneumonia)
    } else if(num == "worst") {
      place <- max(pneumonia)
    } else {
      place <- pneumonia[order(pneumonia)]
      place <- pneumonia[num]
    }
    place <- which(searcher$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == place)
    hospitals <- searcher$Hospital.Name
    hospital <- hospitals[place]
    hospital <- hospital[order(hospital)]
    return(hospital[1])
  }
}