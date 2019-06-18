######################################################
#                                                    #
# Coursera:                                          #
# Getting and Cleaning Data                          #
#                                                    #
# Week 4 Project                                     #
#                                                    #
# File name: run_analysis.R                          #
#                                                    #
#                                                    #
######################################################

# calling libraries

library(dplyr)

######################################################
# step 1: Downloading data file                      #
######################################################

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- download.file(url, destfile = "data.zip")
unzip("data.zip")



######################################################
# Step 2: Read Data                                  #
######################################################

# 1- Reading Features and Activity_labels data
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("id", "featureName"))
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")

# 2- Reading Train data
Subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train  <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train  <- read.table("./UCI HAR Dataset/train/y_train.txt")


# 3- Reading Test data
Subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")


######################################################
# Tasks
######################################################

# We should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
X_all <- rbind(X_train, X_test)
Y_all <- rbind(Y_train, Y_test)
Sub_total <- rbind(Subject_train, Subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# fitering selected data
selected_var <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
X_all <- X_all[,selected_var[,1]]

# 3. Uses descriptive activity names to name the activities in the data set.
colnames(Y_all) <- "activity"
Y_all$activitylabel <- factor(Y_all$activity, labels = as.character(activity[,2]))
activitylabel <- Y_all[,-1]

# 4. Appropriately labels the data set with descriptive variable names. 
colnames(X_all) <- features[selected_var[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
colnames(Sub_total) <- "subject"
complet <- cbind(X_all, activitylabel, Sub_total)
complet_mean <- complet %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(complet_mean, file = "./Getting and cleanining data/assignment/tidydata.txt", row.names = FALSE, col.names = TRUE)
write.table(selected_var[ ,2], file = "./Getting and cleanining data/assignment/selected variables.txt", row.names = TRUE, col.names = TRUE)
