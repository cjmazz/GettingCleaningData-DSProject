
# This script reads dataset files from the the data linked to from the course website that represents data collected from the 
# accelerometers from the Samsung Galaxy S smartphone.  Download link is here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This code is written such that the file is separately downloaded and stored in a local drive and that the working directory is set to the location 
# where "UCI HAR Dataset" is a direct sub-folder in the working directory that is set.
library(data.table)
library(dplyr)

makeTidyDataSet <- function(s = "test") {
  # Read the test or train data set and apply the descriptive rown names (requirement #4) of features to it.
  dataSet <- setNames(data.table(read.table(gsub("test",s,"./UCI HAR Dataset/test/X_test.txt"),sep="")),as.character(featureNames$V2))
 
  # Reduce the feature set for the table to only -mean() and -std() features/columns (plus 'userid' and 'activity' previously added)
  # This is requireemnt #2
  cols <- grep("-mean\\(\\)$|-std\\(\\)$", featureNames$V2)
  reducedDataSet <- dataSet[,..cols]
  
  # Add the subject into a new column - keep as ID's as they are anonymous - necessary to meet requirement #5
  testSubject <- read.table(gsub("test", s, "./UCI HAR Dataset/test/subject_test.txt"),sep="")
  reducedDataSet[,subjectid := testSubject$V1]
  
  #Make a lookup table for the type of activity
  activityLookup <- read.table("./UCI HAR Dataset/activity_labels.txt",sep="")
  lookUpActivity <- setNames(as.character(activityLookup$V2), activityLookup$V1)
  
  # Add the activity into a new column using the descriptive name as the label (not the number) - requirement #3
  testY <-read.table(gsub("test", s,"./UCI HAR Dataset/test/y_test.txt"))
  res <- data.frame(lapply(testY,function(i) lookUpActivity[i]))
  
  #return the dataset to the caller
  return(reducedDataSet[,activity := res$V1])
}

###  Main Scrip ###

# Read the features into a table called featureNames
featureNames <- read.table("./UCI HAR Dataset/features.txt",sep="")

reducedTestSet <- makeTidyDataSet("test")
reducedTrainingSet <- makeTidyDataSet("train")

#Combine the datasets into one (requirement #1)
fullTidyDataSet <- rbind(reducedTrainingSet,reducedTestSet)

#Write out to a file
write.csv(fullTidyDataSet,"./tidyOriginal.csv", row.names=FALSE)

# Now create the summarized dataset (requirement #5)
by_usr_act <- fullTidyDataSet %>% group_by(subjectid,activity)
finalTidy <- summarize_all(by_usr_act,list(mean))
write.csv(finalTidy, "./summaryData.csv", row.names=FALSE)

##ship it!
