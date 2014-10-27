require("data.table")
require("reshape2")

#####################################################################
# 0 - Loading raw data                                              #
#####################################################################
features_labels   <- read.table("./features.txt")[,2]
activity_labels   <- read.table("./activity_labels.txt")[,2]

trainX <- read.table("./train/X_train.txt")
trainY <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")

testX <- read.table("./test/X_test.txt")
testY <- read.table("./test/y_test.txt")
testSubject <- read.table("./test/subject_test.txt")

##############################################################################################
# 1 - Merges the training and the test sets to create one data set.                          #
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. #
# 3. Uses descriptive activity names to name the activities in the data set                  #
##############################################################################################
filterIndices <- grep("mean\\(\\)|std\\(\\)", features_labels)

names(trainX) = features_labels
names(testX)  = features_labels

testX = testX[,filterIndices]
testY[,2] = activity_labels[testY[,1]]
names(testY) = c("ActivityID", "ActivityLabel")
names(testSubject) = "subject"

trainX = trainX[,filterIndices]
trainY[,2] = activity_labels[trainY[,1]]
names(trainY) = c("ActivityID", "ActivityLabel")
names(trainSubject) = "subject"

mergedTrain <- cbind(as.data.table(testSubject), testY, testX)
mergedTest  <- cbind(as.data.table(trainSubject), trainY, trainX)
mergedData  <- rbind(mergedTest, mergedTrain)


#################################################################
# 4. Appropriately labels the data set with descriptive names.  #
#################################################################

setnames(mergedData,names(mergedData),gsub('Acc',"Acceleration",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('Acc',"Acceleration",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('GyroJerk',"AngularAcceleration",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('Gyro',"AngularSpeed",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('Mag',"Magnitude",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('^t',"TimeDomain.",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('^f',"FrequencyDomain.",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('\\.mean',".Mean",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('\\.std',".StandardDeviation",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('Freq\\.',"Frequency.",names(mergedData)))
setnames(mergedData,names(mergedData),gsub('Freq$',"Frequency",names(mergedData)))

#########################################################################################################################
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.  #
#########################################################################################################################

labels <- c("subject", "ActivityID", "ActivityLabel")
data_labels <- setdiff(colnames(mergedData), labels)
melt_data <- melt(mergedData, id = labels, measure.vars = data_labels)
data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)
write.table(data, file = "./data.txt")