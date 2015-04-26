#   You should create one R script called run_analysis.R that does the following. 
#   1. Merges the training and the test sets to create one data set.
#   2. Extracts only the measurements on the mean and standard deviation for each measurement.
#   3. Uses descriptive activity names to name the activities in the data set
#   4. Appropriately labels the data set with descriptive variable names. 
#   5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# initiate necessary libraries
library(plyr)

# read the necessary files into data frames
setwd("UCI HAR Dataset")

# train tables
X_train <- read.table("./train/X_train.txt")
Y_train <- read.table("./train/y_train.txt")

#test tables
X_test <- read.table("./test/X_test.txt")
Y_test <- read.table("./test/y_test.txt")

# Read the features to the data sets
features <- read.table("./features.txt")
ActivityLables <- read.table("./activity_labels.txt")

# Reading the subjects of the study making up the data set
Subject_train<-read.table("./train/subject_train.txt")
Subject_test<-read.table("./test/subject_test.txt")



####   task 1 : Merges the training and the test sets to create one data set.
#               I have included merging the subjects here

X <- rbind(X_train,X_test)
Y <- rbind(Y_train,Y_test)
Subject <- rbind(Subject_train,Subject_test)


####   task 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 

# add column names with the values in the features file
colnames(X) <- c(as.character(features[,2]))
colnames(ActivityLables) <- c("Activity", "ActivityLabel")

# Finding all columns with mean values (i.e. column name has "mean()" in it)
mean <- grep("mean()",colnames(X),fixed=TRUE)

# Finding all columns with standard deviation values (i.e. column name has "std()" in it)
std <- grep("std()",colnames(X),fixed=TRUE)

mean_std <- X[,c(mean,std)]



####   task 3 : Uses descriptive activity names to name the activities in the data set
#               I have included giving column name on the subject table here

Activities <- cbind(Y,mean_std)
colnames(Activities)[1] <- "Activity"

# Joining in labels on the activites
Activities = merge(ActivityLables, Activities, by.x="Activity", by.y="Activity", all=TRUE)
# removing unnecessary columns
Activities$Activity <- NULL
Activities$Label.x <- NULL
Activities$Label.y <- NULL

colnames(Subject)[1] <- "Subject"




####   task 4 : Appropriately labels the data set with descriptive variable names. 

Activities_cols = colnames(Activities)

for (i in 1:length(Activities_cols)) 
{
  Activities_cols[i] = gsub("\\()","", Activities_cols[i]) # remove all brackets
}

colnames(Activities) = Activities_cols ;



####   task 5. From the data set in step 4, creates a second, independent tidy data set 
#              with the average of each variable for each activity and each subject.

# adding the subjects to the dataset
Activities_w_Subj <- cbind(Subject,Activities)

# making final result on first numerical column, i.e. no 3
FinalResult <- aggregate( Activities_w_Subj[,3] ~ Subject+ActivityLabel, data = Activities_w_Subj , FUN= "mean" )

#adding all the other columns, i.e. from col 4 and on
for(i in 4:ncol(Activities_w_Subj)){
  FinalResult[,i] <- aggregate( Activities_w_Subj[,i] ~ Subject+ActivityLabel, data = Activities_w_Subj, FUN= "mean" )[,3]
}

colnames(FinalResult) = colnames(Activities_w_Subj)

setwd("..")

write.table(FinalResult, file = "FinalData.txt")

