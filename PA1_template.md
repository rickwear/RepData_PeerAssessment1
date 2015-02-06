---
title: "Peer Assessment 1"
author: "Rick Wear"
date: "Thursday, February 05, 2015"
output: html_document
---
#Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Data
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

###Loading and preprocessing the data

```r
setwd("C:/general/education/dataScience/05-ReproducibleResearch/PA1")
act <-read.csv("activity.csv", header=TRUE, colClasses=c("numeric", "Date","numeric"))
```

###What is mean total number of steps taken per day?

####Step 1 - Total Number of Steps Per Day ...  

#####Only consider the complete cases within the data ...  
  

```r
actCompIndex <- complete.cases(act)

actComp <- act[actCompIndex, ]
```

#####Get the total steps summed over each day ...  


```r
actSPD <- as.data.frame(tapply(actComp$steps, INDEX = actComp$date, FUN = "sum", na.rm = TRUE))

colnames(actSPD) <-"steps"
```

####Step 2 - Plot the Histogram ...

```r
hist(actSPD$steps, main = "Total Number of Steps Taken Each Day", 
    xlab = "Total Number of Steps Per Day", ylab = "Frequency", 
    breaks = 20, xlim = c(0, 25000), ylim = c(0, 12), col = "blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

####Step 3 - Calculate the Mean and Median of the Total Number of Steps Taken Each Day ...  


```r
meanSteps <- mean(actSPD$steps, na.rm = TRUE)
medianSteps <- median(actSPD$steps, na.rm = TRUE)
```
The mean of the total steps per day is 10766.19 and the median is 10765.00.

###What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
actASI <- as.data.frame(tapply(actComp$steps, INDEX = actComp$interval, FUN = "mean", na.rm = TRUE))
colnames(actASI) <- "average"
```


```r
actASI$interval <- rownames(actASI)
plot(actASI$interval, actASI$average, type = "l",
     xlab = "Time Interval ID",
     ylab = "Average number of steps",
     main = "Average Number of Steps by 5 Minute Interval", 
     col = "blue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
actASI_MAX <- actASI[(actASI$average == max(actASI$average)), ]
maxStepsInterval <- actASI_MAX[1, 2]
maxSteps <- actASI_MAX[1, 1]
```
The 5-minute interval, on average across all the days in the dataset, having the maximum number of steps is interval number 835 and has a value of 206.17 steps.

###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
actNA <- nrow(act) - nrow(na.omit(act))
```
There are 2304 missing values in the original dataset.

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
tmpCount <- act[!actCompIndex, ]
tmpCount[, 1] <- 1
tmpSPDFix <- as.data.frame(tapply(tmpCount$steps, INDEX = tmpCount$date, FUN = "sum", na.rm = TRUE))
daysMissing <- nrow(tmpSPDFix)
daysMissingMean <- mean(tmpSPDFix[,1])
```
There are 8 days in which data are missing an average of 288 readings.


```r
actFixNA <- act
actFixNA[!actCompIndex, 1] <- medianSteps / 288
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
actSPDFix <- as.data.frame(tapply(actFixNA$steps, INDEX = actFixNA$date, FUN = "sum", na.rm = TRUE))
colnames(actSPDFix) <- "steps"
hist(actSPDFix$steps, main = "Total Number of Steps Taken Each Day", 
    xlab = "Total Number of Steps Per Day", ylab = "Frequency", 
    breaks = 20, xlim = c(0, 25000), ylim = c(0, 20), col = "blue")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
meanStepsFix <- mean(actSPDFix$steps, na.rm = TRUE)
medianStepsFix <- median(actSPDFix$steps, na.rm = TRUE)
```
The mean of the total steps per day is now 10766.03 and the median is 10765.00. Adding in the fix data had no effect on these values but the number of days with the median number of steps has increased as expected by the missing 8 days.

###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
actDay <- actFixNA

actDay$weekPeriod <- grepl("^S", weekdays(actDay$date)) 
actDay[actDay$weekPeriod == "FALSE", 4] <- "weekday"
actDay[actDay$weekPeriod == "TRUE", 4] <- "weekend"
head(actDay)
```

```
##      steps       date interval weekPeriod
## 1 37.37847 2012-10-01        0    weekday
## 2 37.37847 2012-10-01        5    weekday
## 3 37.37847 2012-10-01       10    weekday
## 4 37.37847 2012-10-01       15    weekday
## 5 37.37847 2012-10-01       20    weekday
## 6 37.37847 2012-10-01       25    weekday
```

- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
actDaySummary <- aggregate(actDay$steps, by =list(actDay$weekPeriod, actDay$interval), FUN = mean, na.rm = TRUE)
colnames(actDaySummary) <- c("weekPart", "interval", "steps")
head(actDaySummary)
```

```
##   weekPart interval    steps
## 1  weekday        0 7.006019
## 2  weekend        0 4.672309
## 3  weekday        5 5.383796
## 4  weekend        5 4.672309
## 5  weekday       10 5.139352
## 6  weekend       10 4.672309
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
ggplot(actDaySummary, aes(x=interval, y=steps)) + 
                      geom_line(colour="blue") + 
                      facet_wrap(~weekPart, ncol= 1) + 
                      xlab("Time Interval") +
                      ylab("Average number of steps") +
                      ggtitle("Average Steps By Time Interval") +
                      theme_bw() +
                      theme(strip.background = element_rect(colour="#000000", fill="#FFD699"),
                            panel.background = element_rect(fill="#FFFFFF"))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

The implication of the last plot is that user activity is more evenly spread throughout the enitire day at weekends as compared to weekdays where the major activity takes place in the earlier part of the day. In general users are more active at the weekend (mean = 42.3658854) than they are during the week (mean = 35.6100309).
