library(data.table)
library(reshape2)

dirFile <- "./UCI HAR Dataset"
tidyDatasetFile <- "./tidy-dataset.txt"
tidyDatasetFileAVG <- "./tidy-datasetAVG.csv"
tidyDatasetFileAVGtxt <- "./tidy-datasetAVG.txt"

x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)

x <- rbind(x_train, X_test)
y <- rbind(y_train, y_test)
z <- rbind(subject_train, subject_test)


features <- read.table("./UCI HAR Dataset/features.txt")
names(features) <- c('feat_id', 'feat_name')
index_features <- grep("-mean\\(\\)|-std\\(\\)", features$feat_name)
x <- x[, index_features]
names(x) <- gsub("//|//", "", (features[index_features, 2]))


activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activities) <- c('act_id', 'act_name')
y[, 1] = activities[y[, 1], 2]
names(y) <- "Activity"
names(z) <- "Subject"
tidyDataSet <- cbind(z,x,y)


n <- tidyDataSet[, 3:dim(tidyDataSet)[2]]
tidyDataSetAVGSet <- aggregate(n,list(tidyDataSet$Subject, tidyDataSet$Activity), mean)

names(tidyDataSetAVGSet)[1] <- "Subject"
names(tidyDataSetAVGSet)[2] <- "Activity"
write.table(tidyDataSet, tidyDatasetFile, sep="")
write.csv(tidyDataSetAVGSet, tidyDatasetFileAVG, sep="")