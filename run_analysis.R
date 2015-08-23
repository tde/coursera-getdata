library("data.table")

if (!file.exists("UCI HAR Dataset")) {
  # download the data
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  zipfile="UCI_HAR_data.zip"
  download.file(fileURL, destfile=zipfile, method="auto")
  unzip(zipfile)
}

setwd("./UCI HAR Dataset")

#Load Train data
features     = read.table('./features.txt',header=FALSE);
activityType = read.table('./activity_labels.txt',header=FALSE); 
subject = read.table('./train/subject_train.txt',header=FALSE); 
x  = read.table('./train/x_train.txt',header=FALSE); 
y  = read.table('./train/y_train.txt',header=FALSE); 

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(x) = features[,2]; 
colnames(y) = "activityId"

# Merge Test data
trainingData = cbind(y, subjectTrain, x);

# Load Test data
subject = read.table('./test/subject_test.txt',header=FALSE);
x = read.table('./test/x_test.txt',header=FALSE); 
y = read.table('./test/y_test.txt',header=FALSE); 

# Assign column names to the test data imported above
colnames(subject)     = "subjectId";
colnames(x)  = features[,2]; 
colnames(y)  = "activityId";

# Merge Tarin data
testData = cbind(y, subject, x);

# Combine training and test data to final data set
finalData = rbind(trainingData, testData);

#Convert column Names
colNames  = colnames(finalData); 

#Extract only the measurements on the mean and standard deviation for each measurement. 
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# Create a new table without the activityType column
finalData  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalData[,names(finalData) != c('activityId','subjectId')], 
                        by=list(activityId=finalData$activityId,subjectId = finalData$subjectId), 
                        mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType, by='activityId', all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, "../tidy.txt", row.names=FALSE, sep="\t");
