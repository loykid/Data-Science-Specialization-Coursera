getwd()
setwd("C:/Users/User/Desktop")
library(data.table)
outcome <- read.csv("outcome-of-care-measures.csv")
outcome
head(outcome)
names(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
summary(outcome)
str(outcome)
names(outcome)
 
#---- Question 2 -----

# Finding the best hospital in a state
best <- function(state, outcome) {
  # Reading the data
  setwd("C:/Users/User/Desktop")    
  data <- read.csv("outcome-of-care-measures.csv" , header =TRUE, stringsAsFactors=F)          
  outcomes <- c("heart attack" , "heart failure" , "pneumonia") 
  # Check validity of the inputs
  if(!(state %in% unique(data$State))) stop("Invalid State")
  if(!(outcome %in% outcomes)) stop("Invalid Outcome")
  
  # Checking the 'state' input
  St <- data[data$State== state,]
  # align 'outcome' input to the original data
  names(St)[c(11, 17, 23)] <- outcomes
  # Calculate min based on 'outcome' input, Getting the hospital name also
  answer <- St[St[,outcome] == min(St[,outcome]), ][2] 
  # Sorting the hospital names
  # with() - evaluates an R experssion and modifies/making a copy of the data
  final <- answer[with(answer, order(Hospital.Name)), ]
  final[1]   
}

best("TX","heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
best("BB", "heart attack")
# Error in best("BB", "heart attack") : Invalid State


#---- Question 3 ----

# Ranking hospitals by outcome in a a state
rankhospital<- function(state, outcome, num = "best") {
  # Reading the data
  setwd("C:/Users/User/Desktop")    
  data <- read.csv("outcome-of-care-measures.csv" , header =TRUE, stringsAsFactors=F)          
  outcomes <- c("heart attack" , "heart failure" , "pneumonia") 
  # Check validity of the inputs
  if(!(state %in% unique(data$State))) stop("Invalid State")
  if(!(outcome %in% outcomes)) stop("Invalid Outcome")
  # Filtering the data with subset
  data1 <- subset(data, State == state)
  # Choosing the columns based on 'outcome'
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  # Converting the columns to numeric values
  data1[ ,colnum] <- as.numeric(data1[ ,colnum])
  # ordering the columnrs
  finaldata <- data1[order(data1[ ,colnum],data1[,2]), ]
  # Filtering out those NA values
  finaldata <- finaldata[(!is.na(finaldata[ ,colnum])),]
  
  if(num == "best"){
    num <- 1
  }            
  else if (num == "worst"){
    num <- nrow(finaldata)
  }      
  return(finaldata[num,2])
}

rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
rankhospital("NC", "heart attack", "worst")
# [1] "WAYNE MEMORIAL HOSPITAL"

#---- Question 4 ----
rankall<- function(outcome, num = "best")
{
  library(dplyr)
  library(magrittr)
  data <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  
  # Check validity of the inputs
  if(!(outcome %in% outcomes)) stop("Invalid Outcome")
  
  # Choosing the columns based on 'outcome'
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  # Changing values to numeric
  data[ ,colnum] <- as.numeric(data[ ,colnum])
  
  # Removing NA values
  data <- data[!is.na(data[,colnum]),]
  
  # Splitting the data
  data <- split(data, data$State)
  
  # Create a function to create a data table
  ans <- lapply(data, function(x, num) {
    x <- x[order(x[,colnum], x$Hospital.Name),]
    
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}



head(rankall("heart attack", 20), 10)
#                               hospital  state
# AK                                <NA>    AK
# AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# CA               SHERMAN OAKS HOSPITAL    CA
# CO            SKY RIDGE MEDICAL CENTER    CO
# CT             MIDSTATE MEDICAL CENTER    CT
# DC                                <NA>    DC
# DE                                <NA>    DE
# FL      SOUTH FLORIDA BAPTIST HOSPITAL    FL

best("SC", "heart attack")
# [1] "MUSC MEDICAL CENTER"

best("NY", "pneumonia") 
# [1] "ALBANY MEDICAL CENTER HOSPITAL"

rankhospital("NY", "heart attack", 7)
# [1] "BELLEVUE HOSPITAL CENTER"

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
# [1] "CASTLE MEDICAL CENTER"

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
# [1] "BERGEN REGIONAL MEDICAL CENTER"

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
# [1] "RENOWN SOUTH MEADOWS MEDICAL CENTER"

rankhospital("WA", "heart attack", 7)
# [1] "YAKIMA VALLEY MEMORIAL HOSPITAL"

rankhospital("NC", "heart attack", "worst")
# [1] "WAYNE MEMORIAL HOSPITAL"

best("AK", "pneumonia")
# [1] "PROVIDENCE ALASKA MEDICAL CENTER"

best("NY", "pneumonia")
# [1] "ALBANY MEDICAL CENTER HOSPITAL"