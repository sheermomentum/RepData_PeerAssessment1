#get data
require("downloader")
download("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", dest="RepData_PeerAssessment1", mode="wb") 
unzip ("repdata%2Fdata%2Factivity.zip", overwrite="TRUE", exdir = ".")
data<-read.csv("activity.csv")

##What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
steps<-aggregate(data$steps, by=list(date=data$date), FUN=sum, simplify=TRUE)

#If you do not understand the difference between a histogram and a barplot, research the
#difference between them. Make a histogram of the total number of steps taken each day
plot(x=as.POSIXct(steps$date), y=steps$x, type="h", xlab="Date", ylab="Steps")

#Calculate and report the mean and median of the total number of steps taken per day
mean(steps$x, na.rm = TRUE)
median(steps$x, na.rm = TRUE)

##What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all days (y-axis)
intervals<-aggregate(data$steps, list(interval=data$interval), FUN=mean, simplify=TRUE, na.action=na.omit, na.rm=TRUE)
plot(x=intervals$interval, y=intervals$x, type="l", xlab="Interval", ylab="Steps")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
intervals[which.max( intervals[,2] ), ]

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values 
#(coded as NA). The presence of missing days may introduce bias into some calculations or 
#summaries of the data.

#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
nrow(data[!complete.cases(data),])

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the 
#mean/median for that day, or the mean for that 5-minute interval, etc.

#Create a new dataset that is equal to the original dataset but with the 
#missing data filled in.

completeData<-data
for (i in 1:nrow(completeData)){
  if(is.na(completeData[i, 1])){
    completeData[i, 1] <- intervals[intervals$interval == completeData[i, 3], 2]    
  }
}


#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? What is the 
#impact of imputing missing data on the estimates of the total daily number of steps?
steps2<-aggregate(completeData$steps, by=list(date=completeData$date), FUN=sum, simplify=TRUE)
plot(x=as.POSIXct(steps2$date), y=steps2$x, type="h", xlab="Date", ylab="Steps")
mean(steps2$x, na.rm = TRUE)
median(steps2$x, na.rm = TRUE)

  #make a table showing dates, steps with NAs, steps without NAs
require(sqldf)
dateCompare<-sqldf("select * from steps left join steps2 using (date, date)")
colnames(dateCompare)<-c("Date", "With NAs", "WithoutNAs")
print(dateCompare)
  #make a table showing mean and median with NAs, without NAs
avgCompare<-data.frame()
avgCompare[1,1]<-"Mean"
avgCompare[1,2]<-mean(steps$x, na.rm=TRUE)
avgCompare[1,3]<-median(steps$x, na.rm=TRUE)
avgCompare[2,1]<-"Median"
avgCompare[2,2]<-mean(steps2$x, na.rm=TRUE)
avgCompare[2,3]<-median(steps2$x, na.rm=TRUE)
colnames(avgCompare)<-c("Measure", "With NAs", "Without NAs")
print(avgCompare)



##Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with 
#the filled-in missing values for this part.

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
completeData$day<-weekdays(as.POSIXct(completeData[,2]))
completeData[which(completeData$day=='Saturday'),5]<-"weekend"
completeData[which(completeData$day=='Sunday'),5]<-"weekend"
completeData[which(completeData$day=='Monday'),5]<-"weekday"
completeData[which(completeData$day=='Tuesday'),5]<-"weekday"
completeData[which(completeData$day=='Wednesday'),5]<-"weekday"
completeData[which(completeData$day=='Thursday'),5]<-"weekday"
completeData[which(completeData$day=='Friday'),5]<-"weekday"
colnames(completeData)<-c("steps", "date", "interval", "day", "dayType")

factor(completeData$dayType)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, averaged across all 
#weekday days or weekend days (y-axis). See the README file in the GitHub repository 
#to see an example of what this plot should look like using simulated data.
steps3<-aggregate(completeData$steps, by=list(dayType=completeData$dayType, interval=completeData$interval), FUN=mean, simplify=TRUE)
steps3a<-steps3[which(steps3$dayType=="weekday"), ]
steps3b<-steps3[which(steps3$dayType=="weekend"), ]
par(mfrow=c(2,1))
plot(x=steps3a$interval, y=steps3a$x, type="l", xlab="Interval", ylab="Avg Steps Per Day")
plot(x=steps3b$interval, y=steps3b$x, type="l", xlab="Interval", ylab="Avg Steps Per Day")
