# Coursera: Data Science Track: Getting and Cleaning Data
# Peer Assessment

# Source:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# You should create one R script called run_analysis.R that does the following:

# 1. ==========
# Merges the training and the test sets to create one data set.
datFolder <- "UCI HAR Dataset"
datZip <- "Dataset.zip"
if(!file.exists(datFolder)){
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",datZip)
        unzip(datZip)
}
setwd('./UCI HAR Dataset')
# Acquire headers for the data set

feature_names = read.table('features.txt')
head(feature_names)

# Read in files and confirm input

test_x = read.table('test/X_test.txt')
head(test_x)
dim(test_x)

test_y = scan('test/Y_test.txt')
head(test_y)
length(test_y)

test_subjects = scan('test/subject_test.txt')
head(test_subjects)
length(test_subjects)

if ( length(test_y) != length(test_subjects) || length(test_y) != dim(test_x)[1] ) {
        stop('dimensions of test data set inputs do not match')
}

train_x = read.table('train/X_train.txt')
head(train_x)
dim(train_x)

train_y = scan('train/Y_train.txt')
head(train_y)
length(train_y)

train_subjects = scan('train/subject_train.txt')
head(train_subjects)
length(train_subjects)

if ( length(train_y) != length(train_subjects) || length(train_y) != dim(train_x)[1] ) {
        stop('dimensions of training data set inputs do not match')
}

# Set headers on training and test data

names(train_x) = feature_names$V2
names(test_x) = feature_names$V2

# Merge predictors and subject variable for each data set

train_x$subject = train_subjects
test_x$subject = test_subjects

# Merge predictors and outcome variable for each data set

train_x$activity = train_y
test_x$activity = test_y

# Merge data sets

data_set = rbind(train_x, test_x)
head(data_set)
dim(data_set)


# 2. ==========
# Extracts only the measurements on the mean and standard deviation for each measurement.

mean_and_std_columns = grepl( '(-mean\\(\\)|-std\\(\\))', feature_names$V2 )
mean_and_std_columns = append(mean_and_std_columns, TRUE) # keep the subject and activity
mean_and_std_columns = append(mean_and_std_columns, TRUE)

means_and_stds = data_set[, mean_and_std_columns]
names(means_and_stds)
dim(means_and_stds)


# 3. ==========
# Uses descriptive activity names to name the activities in the data set

activity_labels = read.table('activity_labels.txt')
means_and_stds$activity_label = factor(means_and_stds$activity, levels=c(1,2,3,4,5,6), 
                                       labels=activity_labels$V2)


# 4. ==========
# Appropriately labels the data set with descriptive activity names. 
# Understood to mean: Appropriately labels the data set with descriptive variable or 
# feature (column) names

# This was accomplished in Step 1 using the feature_names assigned to names(data.frame)


# 5. ==========
# Creates a second, independent tidy data set with the average of each variable for each activity 
# and each subject. 

# Clarification: column for subject, a column for activity, and columns for each summary variable 
# (so 6*30 as each row represents a unique combination of subject and activity)

# probably there is a more concise way to do this

tidy.frame = data.frame()

subjects = sort( unique(means_and_stds$subject) )
activities = sort( unique(means_and_stds$activity) )

for (subj in subjects) {
        for (act in activities) {
                # subset by subject and activity
                subset = means_and_stds[ means_and_stds$subject==subj & means_and_stds$activity == act, ]
                # get mean values for subject-activity pair, coerce to data.frame
                by_subject_and_activity = as.data.frame( lapply( subset[,1:66], FUN=mean ) )
                # resupply subject, activity and activity label
                by_subject_and_activity$subject = subj
                by_subject_and_activity$activity = act
                by_subject_and_activity$activity_label = activity_labels[act,2]
                # build up tidy dataset
                tidy.frame = rbind(tidy.frame, by_subject_and_activity)
        }
}

setwd('..')
write.table( tidy.frame, file="tidy-data.csv" )