
library(plyr)
library(dplyr)
library(tidyr)

# Loading the data sets
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


# Merge training & test sets adding "Test"&"Train" id-------
x_test_tbl <- x_test %>% tbl_df() #%>% mutate(V562 = "Test")
x_train_tbl <- x_train %>% tbl_df()# %>% mutate(V562 = "Train")
merged_dataset0 <- rbind(x_test_tbl, x_train_tbl)


# Extracts only the measurements on the mean and standard deviation for each measurement----
unique_var_names <- make.names(features$V2, unique=TRUE)# %>% append("Test or Train")

colnames(merged_dataset0) <- unique_var_names

mean_std_unique <- grep("mean|std", unique_var_names, TRUE, value = TRUE)
# merged_dataset1 -> df after extracting the needed variables
merged_dataset1 <- select(merged_dataset0, mean_std_unique)

# merged_dataset1$`Test or Train` <- as.factor(merged_dataset1$`Test or Train`)

# Addition of escriptive activity names----
activities_merged <- rbind(y_test, y_train)
activities_merged$V1 <- as.factor(activities_merged$V1) %>% revalue(c("1" = "WALKING",
                                                                    "2" = "WALKING_UPSTAIRS",
                                                                    "3" = "WALKING_DOWNSTAIRS",
                                                                    "4" = "SITTING",
                                                                    "5" = "STANDING",
                                                                    "6" = "LAYING"))
colnames(activities_merged) <- c("Activity")
# cbind "Activity" column with main data set (merged_dataset1)
activities_plus_merged_dataset1 <- cbind(activities_merged, merged_dataset1)

# Add subjects to merge data set
subjects_bound <- rbind(subject_test, subject_train)
colnames(subjects_bound) <- c("Subject")
# final_merged = data frame after combining subject and activity columns
merged_dataset2 <- cbind(subjects_bound, activities_plus_merged_dataset1 )

# Reorder the variables Subject > Activity > Test or Train
# merged_dataset2 <- select(merged_dataset2, "Subject", "Activity", "Test or Train", everything())
# merged_dataset2 <- select(merged_dataset2, "Subject", "Activity", "Test or Train", everything())

# Appropriately labels the data set with descriptive variable names.
colnames <-colnames(merged_dataset2) %>% make.names(unique=TRUE)
# colnames <- make.names(colnames, unique=TRUE)

# Remove/replace unnecessary characters
colnames_cleaned<-gsub("-", " ", colnames) 
colnames_cleaned<-gsub("\\.", " ", colnames_cleaned) 
colnames_cleaned<-gsub("\\  ", " ", colnames_cleaned) 
colnames_cleaned<-gsub("\\  ", " ", colnames_cleaned) 
colnames_cleaned<-gsub("\\  ", " ", colnames_cleaned) 
colnames_cleaned<-gsub("tBody", "Body", colnames_cleaned) 
colnames_cleaned<-gsub("tGravity", "Gravity", colnames_cleaned) 
colnames_cleaned<-gsub("fBody", "Body", colnames_cleaned) 
colnames_cleaned<-gsub("BodyBody", "Body", colnames_cleaned) 
colnames_cleaned<-gsub("^\\s+|\\s+$", "", colnames_cleaned) 

# Change colnames with cleaned colnames
colnames(merged_dataset2) <- colnames_cleaned

str(merged_dataset2)

# independent tidy_data data set with the average of each 
# variable for each activity and each subject


# tidy_data <- tbl_df(merged_dataset2)
tidy_data <- merged_dataset2    

# Create unique column names, otherwise the summary will give errors
colnames(tidy_data) <- make.names(colnames(tidy_data) , unique=TRUE)

tidy_data <- tbl_df(tidy_data)
# Group the data by subject and activity
grouped <-group_by(tidy_data, Subject, Activity)
# grouped <-group_by(tidy_data, Subject, Activity, Test.or.Train)

# Calculate the mean for all features
grouped_mean <- summarise_all(grouped, funs(mean))

# Reapply the clean column names
colnames(grouped_mean) <- colnames_cleaned

write.table(grouped_mean, file="tidyData.txt", row.names=FALSE, col.names=TRUE)

