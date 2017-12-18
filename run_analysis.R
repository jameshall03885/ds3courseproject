# Week 4 Class Assignment
#
library(dplyr)
library(tidyr)

# This script does the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Step Zero:
# Download https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "UCI_HAR_Dataset.zip"
download.file(fileURL, destfile = destfile, method = "curl")

unzip(destfile)

# read in the test and train feature data
setwd("UCI HAR Dataset")
test_x <- read.table("test/X_test.txt", header = FALSE)
train_x <- read.table("train/X_train.txt", header = FALSE)
activities <- read.table("activity_labels.txt",header = FALSE)
# Add the "activity" column from the test y data

test_y <- read.table("test/y_test.txt", header = FALSE)
test_x$activity <- factor(test_y$V1)

# update the activity column to use descriptive names on levels
levels(test_x$activity) <- activities$V2

# Add subject column
subject <- read.table("test/subject_test.txt", header = FALSE)
test_x$subject <- subject$V1
  
# Rename columns using the documented feature names which are both
# sufficiently descriptive and map directly to the documentation.
#
features <- read.table("features.txt", header = FALSE)
newnames <- c(as.character(features$V2),"activity","subject")
names(test_x) <- newnames

# Reorder Columns to put subject & activity first
neworder <- c(newnames[563],newnames[562],newnames[1:561])
test_x <- test_x[,neworder]

# Same process for "train"

train_x <- read.table("train/X_train.txt", header = FALSE)
train_x <- read.table("train/X_train.txt", header = FALSE)

# Add activity column
train_y <- read.table("train/y_train.txt", header = FALSE)
train_x$activity <- factor(train_y$V1)
levels(train_x$activity) <- activities$V2

# Add subject column
subject <- read.table("train/subject_train.txt", header = FALSE)
train_x$subject <- subject$V1

# Rename columns
newnames <- c(as.character(features$V2),"activity","subject")
names(train_x) <- newnames
# Reorder Columns
neworder <- c(newnames[563],newnames[562],newnames[1:561])
train_x <- train_x[,neworder]

# bind the rows from test & train tables into a merged table
#ls()
alldata <- rbind(train_x, test_x)
rm("train_x")
rm("train_y")

# With training and test data combined, and columns named, select out only those
# columns with mean or std data as well as the subject and activity columns

allmeanstd <- select(alldata,matches("mean|std|activity|subject"))
rm("alldata")
#
# Extract a new table showing the mean of all features for each activity and subject
#
allsummary <- allmeanstd %>%
  group_by( activity, subject) %>%
  summarise_at(vars(`tBodyAcc-mean()-X`:`angle(Z,gravityMean)`),mean)
#
# Rename the columns to indicate the averaging of the mean and std values
#
summarynames <- names(allsummary)
newsummarynames <- c()
for ( nm in summarynames ){
  newnm <- nm
  if ( ! identical("subject",nm) & !identical("activity",nm)) {
    newnm <- paste0("ave(",nm,")")
  }
  newsummarynames <- c(newsummarynames, newnm )
}
names(allsummary) <- newsummarynames

write.table(allsummary, file="summaryAveStdev.txt", row.names = FALSE)

