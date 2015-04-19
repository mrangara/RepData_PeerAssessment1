---
title: "ReproducibleResearch_PeerAssessmentOne"
output: html_document
---
##Introduction  

This assignment will read data obtained from an activity monitoring device and
answer a set of questions provided. The data is provided in the file "activity.csv"
and this script assumes that "activity.csv" file is located in the same
directory where the markdown script is located. The questions posed as part of
the assignment are

1. What is mean total number of steps taken per day?
2. What is the average daily activity pattern ?
3. Impute missing values and devise a strategy for the filling in missing values
4. Depict differences in activity patterns between weekdays and weekends

### Loading Activity.csv file

```r
# Load the activity.csv file. 
# Key Assumption: activity.csv file is in the current directory

activity_df <- read.csv("activity.csv", 
                        header=TRUE,
                        sep=",",
                        stringsAsFactors=FALSE)
```

### Calculating mean total number of steps taken per day  

```r
totalStepsByDate <- tapply(activity_df$steps,activity_df$date,sum,na.rm=TRUE)

totalStepsWithoutDate <- sapply(totalStepsByDate,'[[',1)

hist(totalStepsWithoutDate,
     main = paste("Histogram of", "steps per day"), 
     xlab ="Steps Per Day", 
     ylab = "Number of Days",
     col = "lightblue")

meanTotalSteps <- mean (totalStepsWithoutDate)
medianTotalSteps <- median(totalStepsWithoutDate)

abline (v = meanTotalSteps, lty="dashed", col="blue")
abline (v = medianTotalSteps, col="magenta")

text(x=meanTotalSteps, y=20,labels=paste("Mean = ",round(meanTotalSteps,2)), pos=2,col='blue')
text(x=medianTotalSteps, y=25,labels=paste("Median = ",round(medianTotalSteps,2)), pos=4,col='magenta')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### Plotting the average daily activity pattern  


```r
averageStepsByTimeInterval <- tapply(activity_df$steps,activity_df$interval,mean,na.rm=TRUE)

averageStepsWithoutTimeInterval <- sapply(averageStepsByTimeInterval,'[[',1)
timeIntervals <- levels(factor(activity_df$interval))


#timeIntervals

stepsByIntervalTimeSeries <- ts(averageStepsWithoutTimeInterval)
plot(stepsByIntervalTimeSeries, 
     xlab="Time Interval of Day", 
     ylab="Average Number of Steps", 
     col="blue", 
     xaxt="n")

#Calculation for setting axis labels
maxNumberOfTicks <- length(timeIntervals)
axis(1,at=seq(1,maxNumberOfTicks,20), labels=timeIntervals[seq(1,maxNumberOfTicks,20)],col="black", las=2)

#The time interval that has the maximum average number of steps can be calculated using the following steps

timeIntervalIndex <- which.max(averageStepsWithoutTimeInterval)
abline(v=timeIntervalIndex, lty="dashed", col="red")
axis(3,at=timeIntervalIndex,labels=timeIntervals[timeIntervalIndex],col="red", col.lab="red",las=1)
mtext("(Interval with Maximum average number of Steps)", side=3,at=timeIntervalIndex)
title("Average Daily Activity Pattern", line=2.5)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### Imputing Missing Values  

A number of rows have missing values in the data set. These can be determined by the following code


```r
sum(is.na(activity_df))
```

```
## [1] 2304
```

### Strategy for imputing  

The strategy to replace missing values is to impute these values based on the
average number of steps associated with that interval. In other words, if a
specific interval for a specific date has "NA" for the number of steps, then
this "NA" value is replaced by the average number of steps for all days 
for that interval. The average number of steps for an interval across all
days is calculated in the "Plotting the average daily activity" section


### Creating the new data set with imputed values for steps

```r
require(plyr)
require(dplyr)

# Creating a new dataframe with intervals,average steps during that interval 
# for easier manipulation with dplyr package 

averageTimeInterval_df <- as.data.frame(averageStepsByTimeInterval)

# Adding a new column to the data set that has the imputed
new_activity_df <- mutate(activity_df, 
                          imputedSteps=ifelse(!is.na(steps),
                                              steps,
                                              averageTimeInterval_df$averageStepsByTimeInterval))
```

### Creating a histogram with the imputed values for steps  

```r
totalImputedStepsByDate <- tapply(new_activity_df$imputedSteps,activity_df$date,sum,na.rm=TRUE)

totalImputedStepsWithoutDate <- sapply(totalImputedStepsByDate,'[[',1)

hist(totalImputedStepsWithoutDate,
     main = paste("Histogram of", "imputed steps per day"), 
     xlab ="Imputed Steps Per Day", 
     ylab = "Number of Days",
     col = "lightblue")

meanTotalImputedSteps <- mean (totalImputedStepsWithoutDate)
medianTotalImputedSteps <- median(totalImputedStepsWithoutDate)

abline (v = meanTotalImputedSteps, lty="dashed", col="blue")
abline (v = medianTotalImputedSteps, col="magenta")

text(x=meanTotalImputedSteps, y=20,labels=paste("Mean = ",round(meanTotalImputedSteps,2)), pos=2,col='blue')
text(x=medianTotalImputedSteps, y=25,labels=paste("Median = ",round(medianTotalImputedSteps,2)), pos=4,col='magenta')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

With the imputing strategy chosen, it can be noted that the missing values lower the mean
number of steps per day across the 2 month period thereby reflecting a decreased level of
activity on a daily basis. However, with imputed data, the observations are more centered 
thereby removing the impact of NAs especially in the early days of the observation period.

**Without Imputed Data**  

```r
paste("Mean = ",round(meanTotalSteps,2))
```

```
## [1] "Mean =  9354.23"
```

```r
paste("Median = ",round(medianTotalSteps,2))
```

```
## [1] "Median =  10395"
```

**With Imputed Data**  

```r
paste("Mean = ",round(meanTotalImputedSteps,2))
```

```
## [1] "Mean =  10766.19"
```

```r
paste("Median = ",round(medianTotalImputedSteps,2))
```

```
## [1] "Median =  10766.19"
```

### Differences in activity patterns between weekdays and weekends  

```r
# categorize the activity data into weekday or weekend
new_activity_df <- mutate(new_activity_df, 
                          day=ifelse(as.POSIXlt(date,format="%Y-%m-%d")$wday %in% c(0,6),
                                              "Weekend",
                                              "Weekday"))
new_activity_df <- transform(new_activity_df,day=factor(day))

imputedMeanSummaryByCategory_df <- ddply(new_activity_df, 
                                         .(day,interval), 
                                         summarize, 
                                          mean=round(mean(imputedSteps),2))


# Plotting this using the lattice library

library(lattice)
xyplot(mean ~ interval|day,
       data = imputedMeanSummaryByCategory_df,
       type = "l",
       lty = 1,
       lwd=1,
       cex=1,
       ylab="Average number of steps",
       xlab="Time interval of day",
       main="Activity patterns between weekdays and weekends",
       layout=c(1,2)
       )
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 