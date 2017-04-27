library(dplyr)
library(lubridate)
# Download data and store in a data frame
download.file("https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip",
              "activity.csv.zip")
#library(data.table)
mydata<- tbl_df(read.csv(unzip("activity.csv.zip")))
missing<- which(is.na(mydata$steps))

##Results
###Steps Per Day
#1. Histogram of the total number of steps taken each day


daily<-group_by(filter(mydata,!is.na(steps)),date)
daily_total<- summarize(daily,sum(steps))
names(daily_total)<- c('date','steps')
hist(daily_total$steps,main = "Steps Per Day",xlab="steps",breaks=10)
#2. Mean and median number of steps taken each day
ptable<-summarize(daily_total,mean(steps),median(steps),sd(steps))
knames<-c('mean','median','standard deviation')

#average daily activity pattern
#3. Time series plot of the average number of steps taken[over what time period?]
#	4. The 5-minute interval that, on average, contains the maximum number of steps
daily<-group_by(filter(mydata,interval))
daily<- summarize(daily,mean(steps,na.rm = TRUE),median(steps,na.rm = TRUE), sd(steps,na.rm = TRUE))
colnames(daily)<- c('interval','mean steps','sd steps')
plot(daily$interval,daily$'sd steps',type="l")
Max<-max(daily$steps)
Max
filter(daily,steps == Max)
#clean this up but it's logically correct

#kable(ptable,col.names=knames,caption = "Statistics of Steps Per Day",align = rep("l",2))

#1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# for()
missing<- filter(mydata,is.na(steps))
dim(missing)
for(k in 0:7){
  missing[(288*k+1):(288*k+288),1]<- daily[,2] + rnorm(1,sd = daily$'sd steps') 
}
#Standard devation can be > mean for small means, so insure nonnegative steps.
negative <- which(missing[,1] < 0)
missing[negative,1]<- 0

# Add the imputed values into the full data set
fulldata <- rbind(missing, filter(mydata,!is.na(steps)))
fulldata<- arrange(fulldata,date,interval)
#histogram total daily steps with NAs replaced by imputed values
daily<-group_by(fulldata,date)
daily_total<- summarize(daily,sum(steps))
names(daily_total)<- c('date','steps')
hist(daily_total$steps,main = "Steps Per Day",xlab="steps",breaks=10)
#miniscule difference from data without NAs.  Now look at mean and median
daily<-group_by(filter(mydata,!is.na(steps)),interval)
summarize(daily_total,mean(steps),median(steps),sd(steps))
daily_total
#colnames(daily)<- c('interval','mean steps','sd steps')
plot(daily$interval,daily$'sd steps',type="l")
#I know how many rows I have and that the data is date1--288 interals,date2, 288 intervals, et
#so I don't need the most general possible solution: randomly distributed NAs.

#One more solution for hacking a mode, which works for both numeric & character/factor data:

#Mode <- function(x) {
# ux <- unique(x)
#ux[which.max(tabulate(match(x, ux)))]
#}
