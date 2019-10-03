# Data Cleaning Course Assignment
# Inspiration: https://rpubs.com/ASaeedSh/data-cleaning-project-week-4
#              https://rpubs.com/ninjazzle/DS-JHU-3-4-Final

library(data.table)
library(dplyr)

# Setting the working directory
setwd("C:\\Users\\User\\Desktop\\UCI HAR Dataset")

# Reading features
features <- read.table("./features.txt")

# Reading activity labels
activity_labels <- read.table("./activity_labels.txt")
colnames(activity_labels) <- c('ActivityID','ActivityType')

# Reading our training set data and setting names
xtrain_set <- read.table("./train/X_train.txt")
ytrain_set <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
colnames(xtrain_set) = features[,2]
colnames(ytrain_set) = "ActivityID"
colnames(subject_train) = "SubjectID"

# Reading our test set data and setting names
xtest_set <- read.table("./test/X_test.txt")
ytest_set <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
colnames(xtest_set) = features[,2]
colnames(ytest_set) = "ActivityID"
colnames(subject_test) = "SubjectID"

# 1. Merges the training and the test sets to create one data set.
total_train <- cbind(xtrain_set, ytrain_set, subject_train)
total_test <- cbind(xtest_set, ytest_set, subject_test)
mergeddata <- rbind(total_train,total_test)
names(mergeddata)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
ColNames = colnames(mergeddata)
ColNames
mean_std = (grepl("ActivityID" , ColNames) | 
              grepl("SubjectID" , ColNames) | 
              grepl("mean.." , ColNames) | 
              grepl("std.." , ColNames))
dataset_meanstd <- mergeddata[ , mean_std == TRUE]
names(dataset_meanstd)
names(activity_labels)

# 3. Uses descriptive activity names to name the activities in the data set.
dataset_activity = merge(dataset_meanstd, activity_labels, by='ActivityID', all.x=TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
# Did this at the start to make everything neater

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
# final data set = dataset_activity
final_tidy <- aggregate(. ~ SubjectID + ActivityID, dataset_activity, FUN = mean)
# ordering the data 
final_tidy <- final_tidy[order(final_tidy$SubjectID, final_tidy$ActivityID),]
summary(final_tidy)
# writing the final data set into a text file
write.table(final_tidy, "final_tidy.txt", row.name=FALSE)
