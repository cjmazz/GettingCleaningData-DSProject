
<b>Basic Codebook for the Project 

The datasets included in this repository are generated from the "GetCleanDataRScript.R" file which transform the original datasets that were originating from this site: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The "tidyOriginal.csv" is the originating dataset for what is produced in the "summaryData.csv".  The originating dataset of 'tidyOriginal' consists of a set of mean and std deviation values for a subject (user subject) and a specific activity.  It is a reduced and transformed dataset from the original sources mentioned above.

The variables in both datasets are as follows:
  - subject - A unique identifier of the subject (person) - uniquely identified with a number/ID
  - activity - The specific activity for which measurements were taken
  - *mean(): Mean value of the specific measurement
  - *std(): Standard deviation of the specific measurement

For additional details on the specific type of measurements (such as tGravityAccMag-mean()/-std(), please see the "features_info.txt' file found in the original dataset at the URL above).


Each row of the tidyOriginal.csv contains multiple samples from each subject for a specified activity.  
Each row in summaryData.csv is a rollup and average/mean for each measurement grouped by subject/user and activity.  So one row per subject and activity contains the rollup mean for each of the measurements provided in tidyOriginal.csv.  tidyOriginal contains each specific sample, whereas summaryData.csv is essentially an average of each sample on a per subject, per activity basis.

The R script performs the following from the original datasets:
a) Combines the test and training datasets from the original data files and reduces the features to only measurements ending with "-mean()" or "-std()".
b) Labels each sample (row) with the subject and activity that subject was performing.  The 'activity' is the written activity and not the activity code found in the originating files.  
c) Steps a & b result in the 'tidyOriginal.csv' file which makes dealing with this dataset much easier to work with (although the feature sets are greatly reduced).
d) The 'summaryData.csv' file provides the average for each measurement grouped by subject and activity, thus it is a rolled up average of each sample given by the subject for a specific activity, often there is more than one.  
