#You should create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

rm(list=ls())
library(tidyverse)
data <- read_csv("getdata_data_ss06hid.csv")

setwd('uci_har')
# loading data using readr
features = read_table2('features.txt', col_names=FALSE) 
activityType = read_table2('activity_labels.txt', col_names=FALSE) 
subjectTrain = read_table2('train/subject_train.txt', col_names=FALSE) 
xTrain = read_table2('train/x_train.txt', col_names=FALSE) 
yTrain = read_table2('train/y_train.txt', col_names=FALSE) 
subjectTest = read_table2('test/subject_test.txt', col_names=FALSE) 
xTest = read_table2('test/x_test.txt', col_names=FALSE)
yTest = read_table2('test/y_test.txt', col_names=FALSE)

# adding column names 
colnames(features) <- c('featureId','featureName')
colnames(activityType) <- c('activityId','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features$featureName
colnames(yTrain) <- "activityId"
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features$featureName
colnames(yTest) <- "activityId"

#combining data sets
testData <- cbind(subjectTest, yTest, xTest)
trainData <- cbind(subjectTrain, yTrain, xTrain)
alldata <- rbind(testData, trainData)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

data <- alldata[grepl("subject|mean|std|activity",colnames(data))]

#3. Uses descriptive activity names to name the activities in the data set

data2 <- inner_join(data, activityType)

#4. Appropriately labels the data set with descriptive variable names.

datanames <- names(data2)
datanames <- gsub("\\()", "", datanames)
datanames <- gsub("mean", "Mean", datanames)
datanames <- gsub("std", "StdDev", datanames)
datanames <- gsub("^t", "Time", datanames)
datanames <- gsub("^f", "Freq", datanames)
datanames <- gsub("BodyBody", "Body", datanames)
datanames <- gsub("Acc", "Accel", datanames)
datanames <- gsub("Mag", "Magnitude", datanames)
datanames <- gsub("FreqBodyAcc", "FreqBodyAccel", datanames)
colnames(data2) <- datanames

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data3 <- data2 %>%
    group_by(subjectId, activityType)  %>%
    summarize_at(vars(`TimeBodyAccel-Mean-X`:`FreqBodyGyroJerkMagnitude-MeanFreq`), mean, na.rm = TRUE)
starwars %>%
    summarize_at(vars(TimeBodyAccel-Mean-X:mass), mean, na.rm = TRUE)

write_csv(data3, 'project_getcleandata.csv')
