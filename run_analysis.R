#
library(dplyr)
library(reshape2)

# Get feature names in training and testing feature vectors
feature_labels_con <- file("./UCI HAR Dataset/features.txt")
open(feature_labels_con, "r")

df_features <- read.table(file=feature_labels_con)
feature_labels <- as.character(df_features[, 2])
class(feature_labels)

# Get 6 activity labels in form of character string
activity_labels_con <- file("./UCI HAR Dataset/activity_labels.txt")
open(activity_labels_con, "r")

df_aLabels <- read.table(file=activity_labels_con)
activity_labels <- as.character(df_aLabels[, 2])
#activity_labels
#str(df_aLabels)

# ----------- Process Training Set
# set up input files
train_data_con <- file("./UCI HAR Dataset/train/X_train.txt")  # feature data
train_labels_con <- file("./UCI HAR Dataset/train/y_train.txt")  # feature labels
train_subject_con <- file("./UCI HAR Dataset/train/subject_train.txt")  # subject labels
open(train_data_con, "r")
open(train_labels_con, "r")
open(train_subject_con, "r")

train_subject <- scan(train_subject_con)
train_labels <- scan(train_labels_con)

# replace numerical activity index with text label
ilen <- length(train_labels)
train_activity <- vector(mode="character", length=ilen)   # column to be merged

for (i in 1:ilen) {
  activity_ndx <- train_labels[i]
  train_activity[i] <- activity_labels[activity_ndx]      # text activity label vector
}

init1 <- read.table(file=train_data_con, nrows=10)
classes <- sapply(init1, class)
close(train_data_con)     # closing then opening to re-init the file stream

train_data_con <- file("./UCI HAR Dataset/train/X_train.txt")  # feature data
open(train_data_con, "r")
df_train <- read.table(file=train_data_con,
                       col.names=feature_labels,  # nice column labels for features 
                       colClasses=classes) 

# -----------  Process Testing Set
# set up input files
test_data_con <- file("./UCI HAR Dataset/test/X_test.txt")
test_labels_con <- file("./UCI HAR Dataset/test/y_test.txt")  # feature labels
test_subject_con <- file("./UCI HAR Dataset/test/subject_test.txt")  # subject labels
open(test_data_con, "r")
open(test_labels_con, "r")
open(test_subject_con, "r")

test_subject <- scan(test_subject_con)

test_labels <- scan(test_labels_con)

# replace numerical activity index with text label
jlen <- length(test_labels)
test_activity <- vector(mode="character", length=jlen)  # column to be merged
for (j in 1:jlen) {
  activity_ndx <- test_labels[j]
  test_activity[j] <- activity_labels[activity_ndx]     # text activity label vector
}

init2 <- read.table(file=test_data_con, nrows=10)
classes <- sapply(init2, class)
close(test_data_con)     # closing then opening to re-init the file stream

test_data_con <- file("./UCI HAR Dataset/test/X_test.txt")  # feature data
open(test_data_con, "r")
df_test <- read.table(file=test_data_con, 
                      col.names=feature_labels,  # nice column labels for features 
                      colClasses=classes) 

# ----------- Create Merged Data Frame 
# NOTE: testing set observations followed by training set observations
df_merged <- rbind(df_test, df_train)

# --------------------------------------------------------------------
#
#   df_merged is now a single data set made up from testing and training data with
#    descriptive variable names
#
# --------------------------------------------------------------------
# uncomment to see variable names
#str(df_merged)

# ----------- Isolate All Features Mean and Standard Deviation features 
feat_nms <- names(df_merged)
feat_ndx <- grepl("[Mm]ean()|[Ss]td()", feat_nms)

# >>> NOTE: this eliminates all refs that isn't the result of mean() and std() directly on 
#           independent measurements on the sliding window. For example, operations on
#           GravityMean, JerkMean and MeanFreq are excluded.
#
#           Those removed features may be included by simply setting: Final_ndx <- feat_ndx
feat_ndx_extra <- grepl("[Gg]ravity[Mm]ean|[Mm]ean[Ff]req|[Jj]erk[Mm]ean|[Gg]yro[Mm]ean|[Aa]cc[Mm]ean", feat_nms)
final_ndx <- feat_ndx & !feat_ndx_extra

# reduced_df is the data frame containing only those features that are computed means
#  or standar deviations. NOTE: this includes 
df_reduced <- df_merged[, final_ndx]

# ----------- Add a column each for subject label and activity lable for each observation  
# first: combine test and train activity vectors and subject vectors
Activity <- c(test_activity, train_activity)
Subject <- c(test_subject, train_subject)

# second: pre-append (in order) the two extra named columns 
df_reduced <- cbind(Activity, df_reduced)
df_reduced <- cbind(Subject, df_reduced)

# ----------- Finally, Make "Tidy" Data Set
# the tidy data frame will have 30 subjects x 6 activities = 180 observations
df_tidy <- data.frame()
df_tidy = df_reduced %>% group_by(Subject) %>% group_by(Activity, add=TRUE) %>% summarise_each(funs(mean))

# --------------------------------------------------------------------
#
#   df_tidy is a tidy data set of 180 observations of 68 variables
#
# --------------------------------------------------------------------
# uncomment to see data frame description
#str(df_tidy, 3)

# ----------- Clean up 
# Close all open file connections
close(feature_labels_con)
close(activity_labels_con)
close(train_data_con)
close(train_labels_con)
close(test_data_con)
close(test_labels_con)
close(train_subject_con)
close(test_subject_con)
