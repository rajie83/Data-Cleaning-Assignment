setwd("C:/Users/ec/Desktop/BigData/3 Getting and Cleaning Data/Assignment")

#1. Merges the training and the test sets to create one data set.

trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
table(trainLabel)
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")

testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

joinData <- rbind(trainData, testData)

joinLabel <- rbind(trainLabel, testLabel)

joinSubject <- rbind(trainSubject, testSubject)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("./UCI HAR Dataset/features.txt")

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

#Test content of meanStdIndices

length(meanStdIndices) 

joinData <- joinData[, meanStdIndices]

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

#3. Uses descriptive activity names to name the activities in the data set

activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

#4. Appropriately labels the data set with descriptive activity names. 

names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)

# write out the Merged dataset
write.table(cleanedData, "mergedData.txt") 

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

#Test output
head(result)

# write out the Tidy Dataset file.
write.table(result, "tidyDataWithMeans.txt", row.name=FALSE)

