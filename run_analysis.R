#============================================================================
# ==================== Part 1:  Reading Data ================================
#============================================================================

## Train data
train         <- read.table("./UCI HAR Dataset/train/X_train.txt") 
y_train       <- read.table("./UCI HAR Dataset/train/y_train.txt") 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt") 


## Test data
test         <- read.table("./UCI HAR Dataset/test/X_test.txt") 
y_test       <- read.table("./UCI HAR Dataset/test/y_test.txt") 
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

### Features
features   <- read.table("./UCI HAR Dataset/features.txt")

features_vec <- as.vector(features[[2]]) # putting Features in a vector

#### Activity_labels

activity_labels   <- read.table("./UCI HAR Dataset/activity_labels.txt")


#============================================================================
# =========== Part 2:  Merging the training and the test sets ===============
#============================================================================


# Preparing the train set

names(train) <- features_vec # replacing columns names with the features vector

train <- cbind(subject_train, y_train, train ) # adding subjects and activities data
colnames(train)[1] <- "subject" # renaming subject variable
colnames(train)[2] <- "labels_id" # renaming activity variable


# Preparing the test set

names(test) <- features_vec # replacing columns names with the features vector

test <- cbind(subject_test, y_test, test ) # adding subjects and activities data
colnames(test)[1] <- "subject" # renaming subject variable
colnames(test)[2] <- "labels_id" # renaming activity variable


# Putting train and test together

data <- rbind(train, test)

#============================================================================
# ==================== Part 3: adding activity names ========================
#============================================================================

# renaming activity_labels <= the data set that contains activities codes and labels

colnames(activity_labels)[1] <- "labels_id"
colnames(activity_labels)[2] <- "activity"

# I'll use the "plyr" library to do a left join with the full dada set and the activity_labels set

library(plyr)
data2 <-  join(data, activity_labels, by = "labels_id",  type = "left")

#============================================================================
# ===== Part 4:  Keeping only the Mean and Std for each measurement.  =======
#============================================================================
selcols1 <- grep("mean", names(data2)) 
selcols2 <- grep("std", names(data2)) 

selcols3 <- sort(c(selcols1, selcols2)) # this vector contains the mean and std measurements positions 

selcols <- c(1,564, selcols3) # I have to add to the vector above the variable 1 = subjet and the last=(564) activity

data3 <- data2[,selcols] # subsetting 

# Because the  meanFreq measurement contains the word mean I have to remove them separately

meanFreq <- grep("meanFreq", names(data3)) # meanFreq measurements positions

data3 <- data3[,-meanFreq] # removing meanFreq measurement

#============================================================================
# ==================== Part 5:  Renaming variables ==========================
#============================================================================

names(data3) <- gsub("Acc", "Accelerometer", names(data3))
names(data3) <- gsub("Gyro", "Gyroscope", names(data3))
names(data3) <- gsub("BodyBody", "Body", names(data3))
names(data3) <- gsub("-", "", names(data3))
names(data3) <- gsub("Mag", "Magnitude", names(data3))
names(data3) <- gsub("std", "Std", names(data3))
names(data3) <- gsub("mean", "Mean", names(data3))
names(data3) <- gsub("[\\(\\)]", "", names(data3)) # removing parenthesis
names(data3) <- gsub("^t", "Time", names(data3))
names(data3) <- gsub("^f", "Frequency", names(data3))

#============================================================================
# ==================== Part 6: New tidy data ================================
#============================================================================

#  Tidy data set with the average of each variable for each activity and each subject
tidy_data <- aggregate(. ~ subject+activity, data=data3, FUN=mean)

write.table(tidy_data, "./tidy_data.txt") # saving the txt file


