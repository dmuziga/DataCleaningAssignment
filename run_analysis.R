
######################################################### Question 1############################################
# Load activity labels + features
activityNames <- read.table("activity_labels.txt", header = FALSE)
#CHANGING THE ACtivityName from the  factor to character
activityNames$V2 <- as.character(activityNames$V2)

featureNames <- read.table("features.txt")


#reading the data set for the testdatain R
testset <- read.table("test/X_test.txt", header = FALSE)

testsetname <- read.table(file = "test/y_test.txt", 
                          header = FALSE)
                          

dim(testsetname)

subject_test <- read.table(file = "test/subject_test.txt", 
                           header = FALSE)
                           
#merging the data 

testdataset <- cbind(testset, testsetname,subject_test)

dim(testdataset)

#reading the data set for the training datasetin R
trainset <- read.table( 
        file = "train/X_train.txt",
        header = FALSE)

trainsetname <- read.table(file = "train/y_train.txt", 
                           header = FALSE)
                           

dim(testsetname)

subject_train<- read.table(file = "train/subject_train.txt", 
                           header = FALSE)
                           

dim(trainsetname)
dim(subject_train)
dim(trainset) 

#traindataset <- cbind(trainset,trainsetname,subject_train)

# we equalise the name in order to get the a dataframe which have the same column names

#combine training and test dataset
subject <- rbind(subject_train, subject_test)
activity <- rbind(trainsetname,testsetname)
features <- rbind(trainset,testset )

#Naming Column 
colnames(features) <- featureNames[,2]
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"


#Merge Data
mergeData <- cbind(features,activity, subject)



#===================================== Question 2 ###################



features$V2 <- as.character(features$V2)

# return the position of the value that have mean and  std in the feature column 
featuresNeeded <- grep(".*mean.*|.*std.*", names(mergeData), ignore.case = TRUE)

#returning the column name for mean and std (standard deviation) including the Activity (column number = 562) and Subject (column number = 563)
meansSTD <- c(featuresNeeded, 562,563)

# extract the the column that have the mean and STD
extractedFeatures <-mergeData[, meansSTD ]

class(featuresNeeded)

dim(extractedFeatures)

#===================== Question 3##########################

# Uses descriptive Activity names to name the activities in the dataset

colnames(extractedFeatures) = gsub('-mean', 'Mean',colnames(extractedFeatures))
colnames(extractedFeatures) = gsub('-std', 'Std',colnames(extractedFeatures))
colnames(extractedFeatures) <- gsub('[-()]', '', colnames(extractedFeatures))


# ==========Question 4 #########################

#Appropriately labels the data set with descriptive variable names 
# we remove the Abreviation to put on full name.

colnames(extractedFeatures) <- gsub('Acc', 'Accelerometer',colnames(extractedFeatures))
colnames(extractedFeatures) <- gsub('Gyro', 'Gyroscope',colnames(extractedFeatures))
colnames(extractedFeatures) <- gsub('BodyBody', 'Body',colnames(extractedFeatures))
colnames(extractedFeatures) <- gsub('Subject1', 'Subject',colnames(extractedFeatures))
# ====== Question 5 ######################
library(reshape2)

#I firstly make the Activity and Subject as our Id then we  use dcast to calculate the mean 
newtidy <- melt(extractedFeatures, id.vars = c("Activity", "Subject"))

newtidy <- dcast(newtidy, Activity + Subject ~ variable, fun.aggregate = mean)
newtidy <- newtidy[order(newtidy$Subject,newtidy$Activity),]

#write the result to a file 
write.table(newtidy, file = "TidyDataSet.csv", sep = ",",col.names = TRUE)

