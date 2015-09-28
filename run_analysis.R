# R Script to create Tidy data file from experimental data
rScript <- function() {
  #navigate to experimental data directory
  setwd("~/01 - R/Coursera Getting Data/Class_Project/UCI HAR Dataset")
  # get the feature labels
  features <- read.table('features.txt')
  #get the activity label data for test data set:
  act_label_test <- read.table('./test/y_test.txt')
  #get the test data
  test_data <- read.table('./test/x_test.txt')
  #lets relabel the test data with the features list
  colnames(test_data) <- features$V2
  #lets relabel the Activity labels column
  colnames(act_label_test) <- 'Activity'
  # now translate the activity values into friendly labels
  activities <- read.table('activity_label.txt')
  act_label_test <- sapply(act_label_test, function(x) act_label_test$Activity[x]=activities$V2[x])
  # now bind the Activity label column to the data set
  test_data <- cbind(act_label_test, test_data)
  # read in the subject labels and bind to the test data set
  sub_label_test <- read.table('./test/subject_test.txt')
  # change the column heading
  colnames(sub_label_test) <- 'Subject'
  test_data <- cbind(sub_label_test, test_data)
  # select down to only those columns that calculate mean() and std dev()
  sdcols <- grep('std()', as.character(features$V2), fixed=T, value=T)
  meancols <- grep('mean()', as.character(features$V2), fixed=T, value=T)
  test_data2 <- test_data[,c('Subject', 'Activity', meancols, sdcols)]
  # repeat all of the above for the training data:
  #get the activity label data for train data set:
  act_label_train <- read.table('./train/y_train.txt')
  #get the train data
  train_data <- read.table('./train/x_train.txt')
  #lets relabel the train data with the features list
  colnames(train_data) <- features$V2
  #lets relabel the Activity labels column
  colnames(act_label_train) <- 'Activity'
  # now translate the activity values into friendly labels
  activities <- read.table('activity_label.txt')
  act_label_train <- sapply(act_label_train, function(x) act_label_train$Activity[x]=activities$V2[x])
  # now bind the Activity label column to the data set
  train_data <- cbind(act_label_train, train_data)
  # read in the subject labels and bind to the train data set
  sub_label_train <- read.table('./train/subject_train.txt')
  # change the column heading
  colnames(sub_label_train) <- 'Subject'
  train_data <- cbind(sub_label_train, train_data)
  # select down to only those columns that calculate mean() and std dev()
  sdcols <- grep('std()', as.character(features$V2), fixed=T, value=T)
  meancols <- grep('mean()', as.character(features$V2), fixed=T, value=T)
  train_data2 <- train_data[,c('Subject', 'Activity', meancols, sdcols)]
  # finally combine the two data sets into a single data set
  data <- rbind(test_data2, train_data2)
  # now create the summary data 
  dt3 <- group_by(data, Subject, Activity)
  dt4 <- dt3 %>% summarize_each(funs(mean(.,na.rm=TRUE)))
  write.table(dt4, 'data.txt', row.names = FALSE)
  }