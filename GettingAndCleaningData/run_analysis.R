dir <- "C:\\Users\\laurent\\Google Drive\\repository_git\\3-Getting and Cleaning Data\\quizz"
setwd(dir = dir)
getwd()
#The submitted data set is tidy.
#The Github repo contains the required scripts.
#GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
#The README that explains the analysis files is clear and understandable.
#The work submitted for this project is the work of the student who submitted it.

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "data.zip")
download.file(url, destfile = f)
path <- "data/UCI HAR Dataset"
list.files( path = "data/UCI HAR Dataset")

newdir <- paste(getwd(), path, sep = "/")
newdir
setwd(dir = newdir)
getwd()
list.files()

activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

train_features <- as.vector(features$V2)
#train
train_data <- read.table("train/X_train.txt")
train_subject <- read.table("train/subject_train.txt")
train_activity <- read.table("train/Y_train.txt")

#merging all training files
train <- cbind(train_subject, train_activity, train_data)

#test
test_data <- read.table("test/X_test.txt")
test_subject <- read.table("test/subject_test.txt")
test_activity <- read.table("test/Y_test.txt")

#merging all testing files
test <- cbind(test_subject, test_activity, test_data)

#merging train and test files
data <- rbind(train, test)
    
colnames(data) <- c("subject", "activity", train_features)
names(data)

#find about mean colums
table(grepl("mean", colnames(data)))
#FALSE  TRUE 
#517    46 
table(grepl("std", colnames(data)))
#FALSE  TRUE 
#530    33 

data2 <- data[,grepl("mean|std", colnames(data)) | colnames(data)=="activity" | colnames(data)=="subject"]
dim(data2)
#[1] 10299    81

library(dplyr)
data2 <- data2 %>%
    group_by(activity, subject) %>%
    summarise_all(.funs = c(Mean="mean"))
    
#changing observation values names to meaningful values
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
for(i in 3:length(data2)){
    print(class(data2[i]))
}
data2[,1] <- lapply(data2[,1] , factor)

write.table(data2, "GettingAndCleanindData2.csv", sep = ";", row.name=FALSE)

dim(data2)
View(data2)
str(data2)
