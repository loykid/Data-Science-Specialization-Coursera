library(data.table)
library(dplyr)
library(tidyverse)

# I am by no means an expert, but did have some ideas, just did not know the code to do them
# Inspiration :
# https://rstudio-pubs-static.s3.amazonaws.com/220397_d07534a9d3de4d0d87d7df9036602296.html
# https://paulrobaszewski.wordpress.com/2016/09/02/air-pollution-analysis-and-missing-values/

#---- Question 1----
# 1. Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#    (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
#    takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
#    numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
#    specified in the 'directory' argument and returns the mean of the pollutant across all of 
#    the monitors, ignoring any missing values coded as NA.

# Initially thought that I could load the entire data into a data frame then filter 
# based on the id given
getwd()
setwd("C:/Users/User/Desktop/specdata")
data <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))
names(data)
head(data)
# somehow could not work

# Decided to read files based on id first, then filter based on "pollutant",
# then removed NA values and store in a vector to retain the pertinent values then
# calculate mean
setwd("C:/Users/User/Desktop")
pollutantmean <- function (directory, pollutant, id = 1:332) {
  final <- c()
  for (i in id) {
    files <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
    data <- read.csv(files)
    focus <- data[pollutant]
    final <- c(final, focus[!is.na(focus)])
  }
  mean(final)
}

pollutantmean("specdata", "sulfate", 1:10)
# [1] 4.064128
pollutantmean("specdata", "nitrate", 70:72)
# [1] 1.706047
pollutantmean("specdata", "nitrate", 23)
# [1] 1.280833

#---- Question 2----
# 2. Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. The function should return a data frame 
# where the first column is the name of the file and the second column is the number of
# complete cases. 

# create an empty data frame to store values, and have headers
# loop through the files and get only the necessary ones
# get those that are not NA for sulfate and nitrate
# count using nrow()
# rbind back into our empty dataframe
complete <- function (directory, id = 1:332) {
  results <- data.frame(id = numeric(), nobs = numeric())
  for (i in id) {
    files <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
    data <- read.csv(files)
    final <- data[(!is.na(data$sulfate)),]
    final <- final[(!is.na(data$nitrate)),]
    nobs <- nrow(final)
    results <- rbind(results, data.frame(id = i, nobs = nobs))
  }
  results
}

complete("specdata", 30:25)
#   id nobs
# 1 30  980
# 2 29  749
# 3 28  475
# 4 27  376
# 5 26  613
# 6 25  463
complete("specdata", 3)
#    id nobs
# 1  3  249

#---- Question 3----

# 3. Write a function that takes a directory of data files and a threshold for complete cases
#    and calculates the correlation between sulfate and nitrate for monitor locations where the 
#    number of completely observed cases (on all variables) is greater than the threshold. 
#    The function should return a vector of correlations for the monitors that meet the 
#    threshold requirement. If no monitors meet the threshold requirement, then the function 
#    should return a numeric vector of length 0

corr <- function(directory, threshold = 0){
  data <- list.files(directory, full.names = TRUE)
  correlate = c()
  for (i in 1:332){
    files <- read.csv(data[i], header = TRUE)
    files <- files[complete.cases(files),]
    comp <- nrow(files)
    if (comp > threshold){
      correlate <- c(correlate, cor(files$nitrate, files$sulfate))
    }
  }
  return(correlate)
}

cr <- corr("specdata")
summary(cr)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 

cr <- corr("specdata", 400)
head(cr)
# [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
