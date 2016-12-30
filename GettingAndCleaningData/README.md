##Getting and Cleaning Data

###Instructions

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
Review criteriamoins 
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.
Getting and Cleaning Data Course Projectmoins 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Code processed

#downloading the files
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "data.zip")
download.file(url, destfile = f)

#reading features (subject) and activities labels (activities)
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

#making V2 of features a vector to be used later on
train_features <- as.vector(features$V2)

#importing all the train information
train_data <- read.table("train/X_train.txt")
train_subject <- read.table("train/subject_train.txt")
train_activity <- read.table("train/Y_train.txt")

#merging all training files
train <- cbind(train_subject, train_activity, train_data)

#importing all the test information
test_data <- read.table("test/X_test.txt")
test_subject <- read.table("test/subject_test.txt")
test_activity <- read.table("test/Y_test.txt")

#merging all testing files
test <- cbind(test_subject, test_activity, test_data)

#merging train and test files
data <- rbind(train, test)

#rename data's columns 
colnames(data) <- c("subject", "activity", train_features)

#checking how many colums contain the letters mean and std
#find about mean colums
table(grepl("mean", colnames(data)))
#FALSE  TRUE 
#517    46 
table(grepl("std", colnames(data)))
#FALSE  TRUE 
#530    33 

#keep only columns that contain mean, std and the two firt columns activity and subject
data2 <- data[,grepl("mean|std", colnames(data)) | colnames(data)=="activity" | colnames(data)=="subject"]

#Summarising the data set grouped by subject and activity for each activity and each subject
library(dplyr)
data2 <- data2 %>%
    group_by(activity, subject) %>%
    summarise_all(.funs = c(Mean="mean"))
    
#changing activities values names to meaningful values
data2[data2$activity==1,1] <- "WALKING"
data2[data2$activity==2,1] <- "WALKING_UPSTAIRS"
data2[data2$activity==3,1] <- "WALKING_DOWNSTAIRS"
data2[data2$activity==4,1] <- "SITTING"
data2[data2$activity==5,1] <- "STANDING"
data2[data2$activity==6,1] <- "LAYING"

#changing vaiable names to meaningful names
for(i in 3:length(data2)){
    names(data2)[i] <- gsub("Acc", " accelerometer ", names(data2)[i])
    names(data2)[i] <- gsub("Gyro", " gyroscope ", names(data2)[i])
    names(data2)[i] <- gsub("Jerk", " Jerk ", names(data2)[i])
    names(data2)[i] <- gsub("Mag", " Magnitude ", names(data2)[i])
    names(data2)[i] <- gsub("^[f]", "Fast Fourier ", names(data2)[i])
    names(data2)[i] <- gsub("^[t]", "Time ", names(data2)[i])
    names(data2)[i] <- sub("-", " ", names(data2)[i])
    
    names(data2)[i] <- gsub("BodyBody", "Body", names(data2)[i])
    names(data2)[i] <- gsub("( +)", " ", names(data2)[i])
    
    print(names(data2[,i]))
}

#change variable class for codeBook purpose
data2[,1] <- lapply(data2[,1] , factor)

#export final data set to GettingAndCleanindData.csv
write.table(data2, "GettingAndCleanindData.csv", sep = ";")