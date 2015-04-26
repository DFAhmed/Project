# First load the data
x_train<-read.table("D:/Coursera/GettingandCleaningData/Data/train/X_train.txt")
y_train<-read.table("D:/Coursera/GettingandCleaningData/Data/train/y_train.txt")

x_test<-read.table("D:/Coursera/GettingandCleaningData/Data/test/X_test.txt")
y_test<-read.table("D:/Coursera/GettingandCleaningData/Data/test/y_test.txt")

test_subject<-read.table("D:/Coursera/GettingandCleaningData/Data/test/subject_test.txt")
train_subject<-read.table("D:/Coursera/GettingandCleaningData/Data/train/subject_train.txt")

#merging test and train data

x_merged<-rbind(x_train,x_test)
y_merged<-rbind(y_train,y_test)
subjects_merged<-rbind(train_subject,test_subject)

# Reading features related to mean and std.

feature_names<-read.table("D:/Coursera/GettingandCleaningData/Data/features.txt")

names_idx<-rep(NA,1)
# Getting column names first
k<-1
for(i in 1:nrow(feature_names)){
 if(length(grep('mean',feature_names[i,2]))>0|length(grep('std',feature_names[i,2]))>0){
   names_idx[k]<-feature_names[i,1]
   k<-k+1
 }
}
# Getting only mean and std related data
mean_Data<-x_merged[,names_idx]
# Assigning names to coulmns
colnames(mean_Data)<-feature_names[names_idx,2]
# Merging activity with data
xy_merged<-cbind(y_merged,mean_Data)
# Merging subject to all data (including activity)
all_merged<-cbind(subjects_merged,xy_merged)
# Renaming headers (only first two)
colnames(all_merged)[1]<-'Subject'
colnames(all_merged)[2]<-'Activity'

# Replacing Activity with name..
activity_list<-c('Walking','WalkingUpstair','WalkingDownstair','Sitting','Standing','Laying')
length(activity_list)
for(i in 1:6){
  all_merged$Activity[all_merged$Activity==i]<-activity_list[i]
}
# Creating empty variable name
tidy_data<-data.frame(matrix(NA, nrow = 30, ncol = 81))
# Here Finally i will create tidy data in which COlumn 1 sill be subject name, column 2 will be 
# activity and then the data for each activity corresponding to subject and activity (mean). Since there are 30 subjects and 6 activities 
# there will be in total 30*6=180 rows and there are 79+2 columns in the final data.
k<-1;
for(subject in 1:30){
  for(activity in 1:length(activity_list)){
    idx<-which(all_merged$Activity==activity_list[activity] & all_merged$Subject==subject)
    for(col in 1:81){
      if(col==1 | col==2){
        tidy_data[k,1]<-subject
        tidy_data[k,2]<-activity_list[activity]       
      }else{
        tidy_data[k,col]<-mean(all_merged[idx,col],na.rm=TRUE)
      }
    }
    k<-k+1
  }
}
# Removing () from the names of the column, and changing mean to Mean and std tp Std for better reading
length(names(all_merged))
column_names<-gsub("-mean\\()-","Mean",names(all_merged))
column_names<-gsub("-mean\\()","Mean",column_names)
column_names<-gsub("-std\\()","Std",column_names)
column_names<-gsub("-std\\()-","Std",column_names)
column_names<-gsub("\\()","",column_names)


# Renaming variables
colnames(tidy_data)<-column_names


write.csv(tidy_data,'D:/Coursera/GettingandCleaningData/Data/tidy_data.txt')
