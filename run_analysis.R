setwd("/Users/adhyan/R/Data/UCI HAR Dataset")


#Part 1: Merge test and train data sets



#work on the test data set first

testActivity <- read.table("test/y_test.txt")
names(testActivity) <- c("Activity")
testSubject <- read.table("test/subject_test.txt")
names(testSubject) <- c("Subject")
testSet <- read.table("test/X_test.txt")
features <- read.table("features.txt")

#the features with bandsEnergy() are missing their x,y and z designation
#we will fix this by adding the required values in order to maintain unique columns

features[303:316,2] <- paste(features[303:316,2],"X",sep="-")
features[317:330,2] <- paste(features[317:330,2],"Y",sep="-")
features[331:344,2] <- paste(features[331:344,2],"Z",sep="-")

features[382:395,2] <- paste(features[382:395,2],"X",sep="-")
features[396:409,2] <- paste(features[396:409,2],"Y",sep="-")
features[410:423,2] <- paste(features[410:423,2],"Z",sep="-")

features[461:474,2] <- paste(features[461:474,2],"X",sep="-")
features[475:488,2] <- paste(features[475:488,2],"Y",sep="-")
features[489:502,2] <- paste(features[489:502,2],"Z",sep="-")


#set the variables of the test set
names(testSet) <- features[,2]

#combine the test data into one table
test <- cbind.data.frame(testSubject,testActivity,testSet)



#now work on the training data set

trainActivity <- read.table("train/y_train.txt")
names(trainActivity) <- c("Activity")
trainSubject <- read.table("train/subject_train.txt")
names(trainSubject) <- c("Subject")
trainSet <- read.table("train/X_train.txt")

#set the variables of the train set
names(trainSet) <- features[,2]

#combine the train data into one table
train <- cbind.data.frame(trainSubject,trainActivity,trainSet)


#merge test and train data
dataset <- merge(test,train, by = intersect(names(test),names(train)), all.x = TRUE, all.y= TRUE)


#-------------------------------------------------------------------------------

#Part 2: Extract Measurements on Mean and Standard Deviation

DT <- tbl_df(dataset)
#extracts values that contain "mean()" or "std()"
extracted <- select(.data = DT, Subject, Activity, contains("mean()") | contains("std()"))


#-------------------------------------------------------------------------------

#Part 3: Add descriptive activities to extracted data

DescriptiveActivity <- character(nrow(extracted))

for (i in seq(nrow(extracted))) {
  
  if(extracted$Activity[i]==1) DescriptiveActivity[i] <- "WALKING"
  else if(extracted$Activity[i]==2) DescriptiveActivity[i] <- "WALKING_UPSTAIRS"
  else if(extracted$Activity[i]==3) DescriptiveActivity[i] <- "WALKING_DOWNSTAIRS"
  else if(extracted$Activity[i]==4) DescriptiveActivity[i] <- "SITTING"
  else if(extracted$Activity[i]==5) DescriptiveActivity[i] <- "STANDING"
  else if(extracted$Activity[i]==6) DescriptiveActivity[i] <- "LAYING"
  
}

extracted <- mutate(.data = extracted, Activity = DescriptiveActivity)



#-------------------------------------------------------------------------------

#Part 4: Label dataset with descriptive variable names (completed)

#-------------------------------------------------------------------------------

#Part 5: Create a separate tidy data set with the average of each variable for each activity and subject

activityUnique = unique(extracted$Activity)
subjectUnique = unique(extracted$Subject)
Variables <- names(extracted[-(1:2)]) # feature variable names from extracted data

#initially only contains WALKING
final <- data.frame(colMeans(extracted[extracted$Activity=="WALKING",][,-(1:2)]))

#add means of other activities
for(i in activityUnique) {
  if(i!="WALKING") final <- cbind(final,colMeans(extracted[extracted$Activity==i,][,-(1:2)]))
}

#add means of subjects
for(i in subjectUnique) {
  final <- cbind(final,colMeans(extracted[extracted$Subject==i,][,-(1:2)]))
}

#change the header names
names(final) <- union(activityUnique,subjectUnique)

#download the table
write.table(final, "step5dataset.txt", row.names = FALSE)  
