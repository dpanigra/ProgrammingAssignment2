Course Project Code Book
========================

* Data Set is used from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  (Collected from the assignment instruction)

* Meta data (data definition) of the Data set is explained in "Human Activity Recognition Using Smartphones Data Set" available at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones (Collected from the assignment instruction)

* run_analysis.R performs the following to clean up the data:

** Reads and merges the training and test data sets of X, Y, and Subjects. Merged data of train/X_train.txt and test/X_test.txt is a 10299 (number of instances) by 561 (number of attributes) data frame. Merged data of train/y_train.txt and test/y_test.txt is a 10299 (number of instances) by 1 (number of attribute) data frame with Activity IDs. Merged data of train/subject_train.txt and test/subject_test.txt is a 10299 (number of instances) by 1 (number of attribute) data frame with Subject IDs.

** Reads features.txt. Assigns friendly names to the columns (i.e. feature_id and feature)name). Extracts only the measurements by filtering only the mean and the standard deviation for each measurement. Out of 561 features only 66 are measurements with mean and standard deviation. Collects indices (attribute position) of these 66 measures. Collects names of these 66 attributes. Removes "(" and ")" from the attribute names (e.g. tbodyacc-mean-x, tbodyacc-mean-y, tbodyacc-mean-z, etc.)

** Truncates the data frame (of consolidated train and test X data) to contain 66 attributes with the indices as determined in the above step (contains only mean and standard deviation.) The result is a 10299 (number of instances) by 66 (number of attributes). Assigns friendly attribute names to the data frame with the names collected in the above step.

** Reads activity_labels.txt. Converts the activities values to lowercase. Converts numerical activity value (viz. 1,2,3,4,5,6) to descriptive activity names (viz. walking, walkingupstairs, walkingdownstairs, sitting, standing, and laying) in the data set (Y data set).

** Merges Subject, Activities, and the data. Writes the content to merged_clean_data.txt. Creates a second tidy set with the average of each measurement for each activity and each subject. Assigns friend names Subject and Activity to the first two attributes. Saves the result in data_set_with_the_averages.txt, a 180x68 data frame. Also writes the tidy data frame without row names (in the observations) to data_set_with_the_averages_rnf.txt. The file will be uploaded in the submission box as instructed in the assignment ("Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).")

