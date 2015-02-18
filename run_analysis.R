# Get Data, Clean Data
# Course Project
# February 2015

## preliminary step:  set working directory to parent directory for dataset's 
##  example:  working directory contains directory "UCI HAR Dataset/" 

setwd("c:/users/heather/desktop/datascience/getdatacleandata/course_project/")


##  Checklist quoted from project instructions:
# You should create one R script called run_analysis.R that does the following. 
# 
# done 1) Merges the training and the test sets to create one data set.
# done 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# done 3) Uses descriptive activity names to name the activities in the data set
# done 4) Appropriately labels the data set with descriptive variable names. 
# done 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones


### References
##  class lectures and discussion forum
##  Stack Overflow
#      http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name/28576595#28576595


################################################################################
################################################################################
################################################################################


# load packages

library("dplyr")
library("tidyr")


###  load data

# training set
filename_training_x <- "UCI HAR Dataset/train/X_train.txt"
filename_training_subjects <- "UCI HAR Dataset/train/subject_train.txt"
filename_training_activities <- "UCI HAR Dataset/train/y_train.txt"

# testing set
filename_testing_x <- "UCI HAR Dataset/test/X_test.txt"
filename_testing_subjects <- "UCI HAR Dataset/test/subject_test.txt"
filename_testing_activities <- "UCI HAR Dataset/test/y_test.txt"



# both test & train
filename_feature_labels <-"UCI HAR Dataset/features.txt"
filename_activity_labels <-"UCI HAR Dataset//activity_labels.txt"

###

### training set
# read X training data
training_x <- read.table(filename_training_x)

# read features.txt & use feature labels as column names
column_labels <- read.table(filename_feature_labels)
col_names <- as.character(column_labels$V2)
names(training_x) <- col_names

# read subject ID's and rename column
training_subjects<- read.table(filename_training_subjects)
names(training_subjects) <- "subjectID"

# read activity log, rename column
# later: read labels file; apply labels as col names to log
training_activities <- read.table(filename_training_activities)
names(training_activities) <- "activities"


training_dataset <- cbind(training_subjects, training_activities, training_x)
training_dataset$group <- "train"


#######################
testing_x <- read.table(filename_testing_x)

## add descriptive column names from feature list
names(testing_x) <- col_names


testing_subjects<- read.table(filename_testing_subjects)
names(testing_subjects) <- "subjectID"


# read activity log, rename column
# later in script: read labels file; replace integers with string activity descriptions
testing_activities <- read.table(filename_testing_activities)
names(testing_activities) <- "activities"


# group test data into a single dataset and create a new column to with group (test or train) name
testing_dataset <- cbind(testing_subjects, testing_activities, testing_x)
testing_dataset$group <- "test"



################################################################



###  Merge training and test into one data set



## check that subject ID's are unique to test and train files, before merging
##  (so that two different people's data aren't merged into one identifier)

# there must be a more efficient way of comparing unique ID's
if (length(intersect(unique(training_subjects$V1), unique(testing_subjects$V1))) != 0 ) {
  
  stop("detected subject ID(s) in both test and training data sets")
}


# remove data objects for original (separate) files
rm("training_x",   "training_subjects", "training_activities", "testing_x", "testing_activities", "testing_subjects")



full_dataset <- rbind(training_dataset, testing_dataset)
#names_full_dataset <- names(full_dataset)

### Descriptive activity names
## from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#
# activities are {WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING}



activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", sep=" ", header=FALSE)  # activity identifiers
names(activity_labels) <- c("activity_number", "activity_name")


# translate activity numbers into activity names (lookup table)
#http://stackoverflow.com/questions/10002536/how-do-i-replace-numeric-codes-with-value-labels-from-a-lookup-table
#df$profession.str <- names(profession.code)[match(df$profession, profession.code)]

activities_by_name <- activity_labels$activity_name[match(full_dataset$activities, activity_labels$activity_number)]
full_dataset$activities <- activities_by_name




# make *valid* names for columns (original names have invalid special characters)
valid_names <- make.names(names=names(full_dataset), unique=TRUE, allow_ = TRUE)

# remove extra periods introduced by make.names
##   replace repeated periods or periods at the end of a variable name with "" 
cleaned_names <- sub("\\.(\\.)+|\\.$", "", valid_names)

names(full_dataset) <- cleaned_names

################################################################

### Extract measurements on the mean and std
full_tbl <- tbl_df(full_dataset)
rm("full_dataset")




selected_tbl <- select(full_tbl, subjectID, activities, contains("mean"), contains("std"))

### Descriptive variable names

# subject, activity, and feature names were created in previous steps 




### Create independent tidy data set with avg of each variable for each activity, by each subject
# group by subject


# http://stackoverflow.com/questions/21644848/summarizing-multiple-columns-with-dplyr
tidy_summary <- selected_tbl %>% 
  group_by(subjectID, activities) %>%
  summarise_each(funs(mean))%>% 
  print

View(tidy_summary)
## 


### write tidy data set to .txt file
#  this txt file is uploaded on the course project assignment page
write.table(tidy_summary, file="tidy_summary.txt", col.names=TRUE, row.name=FALSE) 


# to read new tidy dataset table back into R:
check_table <- read.table("tidy_summary.txt", header=TRUE)


##  Generate variable list for codebook: 
write.table(names(tidy_summary), file="forcodebook.txt")
