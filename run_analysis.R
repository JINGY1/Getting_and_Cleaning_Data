# Course Project
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# load file
path <- getwd()
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, file.path(path, "Course_Proj_Data.zip"))
unzip(zipfile="Course_Proj_Data.zip")


# load data
library(data.table)
filepath <- paste(path,"/UCI HAR Dataset", sep='')
activity_label <- fread(file.path(filepath,"activity_labels.txt"), col.names=c("label","activity"))
features <- fread(file.path(filepath,"features.txt"), col.names=c("id","feature_nm"))
features_filter <- grep("(mean|std)\\(\\)", features[, feature_nm])
measurements <- features[features_filter, feature_nm]
measurements <- gsub('[()]', '', measurements)


# load train datasets
train <- fread(file.path(filepath, "train/X_train.txt"))[, features_filter, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
train_activity <- fread(file.path(filepath, "train/Y_train.txt"), col.names = c("activity"))
train_subject <- fread(file.path(filepath, "train/subject_train.txt"), col.names = c("subject_id"))
train_data <- cbind(train_subject, train_activity, train)


# load test datasets
test <- fread(file.path(filepath, "test/X_test.txt"))[, features_filter, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
test_activity <- fread(file.path(filepath, "test/Y_test.txt"), col.names = c("activity"))
test_subject <- fread(file.path(filepath, "test/subject_test.txt"), col.names = c("subject_id"))
test_data <- cbind(test_subject, test_activity, test)


# merge
merge_data <- rbind(train_data, test_data) 
merge_data[["activity"]] <- factor(merge_data[, activity], levels = activity_label[["label"]], labels = activity_label[["activity"]])
merge_data[["subject_id"]] <- as.factor(merge_data[, subject_id])
merge_data <- reshape2::melt(data = merge_data, id = c("subject_id", "activity"))
merge_data <- reshape2::dcast(data = merge_data, subject_id + activity ~ variable, fun.aggregate = mean)

# create new datasets
data.table::fwrite(x = merge_data, file = "tidy_data.txt", quote = FALSE)