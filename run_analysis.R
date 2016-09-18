
## Setting the folder path
path <- file.path("UCI HAR Dataset")

## Reading generic information for both test and train data sets
features <- read.table(paste(path, "features.txt", sep = "/"))
activity_labels <- read.table(paste(path, "activity_labels.txt", sep = "/"))

## Reading test data set
X_test <- read.table(paste(path, "test", "X_test.txt", sep = "/"))
y_test <- read.table(paste(path, "test", "y_test.txt", sep = "/"))
subject_test <- read.table(paste(path, "test", "subject_test.txt", sep = "/"))

## Reading the train data set
X_train <- read.table(paste(path, "train", "X_train.txt", sep = "/"))
y_train <- read.table(paste(path, "train", "y_train.txt", sep = "/"))
subject_train <- read.table(paste(path, "train", "subject_train.txt", sep = "/"))

## Naming columns for each data frame
names(features) <- c("feature_id", "feature_name")
names(activity_labels) <- c("activity_id", "activity_name")
names(subject_test) <- c("subject_id")
names(subject_train) <- c("subject_id")
names(y_test) <- c("activity_id")
names(y_train) <- c("activity_id")
names(X_test) <- features$feature_name
names(X_train) <- features$feature_name

## Creating a complete test data set
test_data_set <- cbind(subject_test, X_test, y_test)

## Creating a complete train data set
train_data_set <- cbind(subject_train, X_train, y_train)

## Creating a complete data set including both test and train data
complete_data_set <- rbind(test_data_set, train_data_set)

## Part 1 is then achieved
## "Merges the training and the test sets to create one data set"

## --------------------
  
## Identifying which columns to keep (mean and std, plus subject_id and 
## activity_id)
col_to_subset <- grepl("mean\\(\\)|std\\(\\)|subject_id|activity_id", 
                       names(complete_data_set))

## Note: as meanfreq is also present in the variable names, it is necessary
## to use \\ in the grepl function to only keep mean()

## Subsetting the data set with only those columns
data_set <- complete_data_set[,col_to_subset]

## Part 2 is then achieved
## "Extracts only the measurements on the mean and standard deviation 
## for each measurement"

## --------------------

## Merging the data_set with activity_labels to have descriptive activity names
## instead of only activity_id (1 to 6 in this case)
data_set <- merge(data_set, activity_labels, by = "activity_id", all.x = TRUE)

## Part 3 is then achieved
## "Uses descriptive activity names to name the activities in the data set"

## --------------------

## Ensuring package are loaded to melt data sets
## install.packages("reshape2")
require(reshape2)
## library(reshape2)

## Melting data set
col_id <- c("activity_id", "subject_id", "activity_name")
col_names <- names(data_set)
col_vars <- col_names[col_names != col_id]
data_melt <- melt(data_set, id = col_id, measure.vars = col_vars)

## Removing duplicate information (activity_id), as we already have the
## activity_name as factor in the data frame
data_melt <- data_melt[, names(data_melt) != "activity_id"]

## Identifying which measurement corresponds to the row (mean or std)
ismean <- grepl("mean\\(\\)", data_melt$variable)
isstd <- grepl("std\\(\\)", data_melt$variable)

## Assigning the measurement to a new variable called "measurement"
## and declaring it as a factor
data_melt$measurement[ismean] <- "mean"
data_melt$measurement[isstd] <- "std"
data_melt$measurement <- as.factor(data_melt$measurement)

## Identifying which sensor was measured (accelerometer or gyroscope)
isacc <- grepl("acc", tolower(data_melt$variable))
isgyro <- grepl("gyro", tolower(data_melt$variable))

## Assigning the sensor used to a new variable called "sensor"
## and declaring it as a factor
data_melt$sensor[isacc] <- "accelerometer"
data_melt$sensor[isgyro] <- "gyroscope"
data_melt$sensor <- as.factor(data_melt$sensor)

## Identifying which component (body or gravity)
isbody <- grepl("body", tolower(data_melt$variable))
isgravity <- grepl("gravity", tolower(data_melt$variable))

## Assigning the component measured to a new variable called "component"
## and declaring it as a factor
data_melt$component[isbody] <- "body"
data_melt$component[isgravity] <- "gravity"
data_melt$component <- as.factor(data_melt$component)

## Identifying the domain (time or frequency)
istime <- grepl("^t", data_melt$variable)
isfrequency <- grepl("^f", data_melt$variable)

## Assigning the domain to a new variable called "domain"
## and declaring it as a factor
data_melt$domain[istime] <- "time"
data_melt$domain[isfrequency] <- "frequency"
data_melt$domain <- as.factor(data_melt$domain)

## Identifying if the variable is a Jerk signal or the "initial" one
## (e.g. just the one filtered, not the raw signal)
## and assigning the value (jekr or initial) to a variable called signal
isjerk <- grepl("jerk", tolower(data_melt$variable))
data_melt$signal[isjerk] <- "jerk"
data_melt$signal[!isjerk] <- "initial"
data_melt$signal <- as.factor(data_melt$signal)

## Identifying the direction (X, Y, Z or magnitude) 
## and assigning it to a variable called dimension
isx <- grepl("X$", data_melt$variable)
isy <- grepl("Y$", data_melt$variable)
isz <- grepl("Z$", data_melt$variable)
ismag <- grepl("Mag", data_melt$variable)

data_melt$dimension[isx] <- "X"
data_melt$dimension[isy] <- "Y"
data_melt$dimension[isz] <- "Z"
data_melt$dimension[ismag] <- "mag"

data_melt$dimension <- as.factor(data_melt$dimension)

## Checking if descriptive variables cover all the initial features
unique_features <- unique(data_melt$variable)
unique_variables <- unique(paste(data_melt$measurement, data_melt$sensor, 
                                 data_melt$component, data_melt$domain, 
                                 data_melt$jerk, data_melt$dimension, sep = ""))
length(unique_features == unique_variables)

## Finalize data set by removing duplicate information (variable)
## as all features are covered by other variables
## and re_ordering the columns in a more sensible / logical way
data_final <- data_melt[,names(data_melt) != "variable"]
data_final <- data_final[,c(1,2,5,6,7,9,8,4,3)]

## Part 4 is then achieved
## "Appropriately labels the data set with descriptive activity names"

## --------------------

## Create a new tidy data set with the average of each variable for each
## combination of subject_id and activity_name
## Store this set into the data frame "tidy_data"
tidy_data <- aggregate(value ~ ., data_final, mean)
dest.file <- file.path("tidydata.txt")
write.table(tidy_data, file = dest.file, sep = "\t", row.names = FALSE)


## Part 5 is then achieved
## "From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject"