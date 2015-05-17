---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Download, extract and store the data in the `activity` data frame. Also transform the date field to `Date` format and print a sample of values to understand the data.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

Histogram of the total number of steps taken day by day:


```r
library(ggplot2)
plot<-qplot(date, weight=activity$steps, data=activity, geom="histogram")
print(plot)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Mean and Median number of steps per day:


```r
mean(tapply(activity$steps, activity$date, sum, na.rm = TRUE))
```

```
## [1] 9354.23
```

```r
median(tapply(activity$steps, activity$date, sum, na.rm = TRUE))
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Plot of the average number of steps, across all days:


```r
avg_steps<-data.frame(cbind(activity$interval,tapply(activity$steps, activity$interval, mean, na.rm = TRUE)))
colnames(avg_steps) <- c("interval","steps")
plot<-ggplot(data=avg_steps,aes(x=interval,y=steps)) +
  geom_line()
print(plot)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The 5-minute interval which contains the maximum number of steps across all the days in the dataset is:


```r
maxtime<-avg_steps[which.max(avg_steps$steps),"interval"]
strftime( as.POSIXct(Sys.Date()) + as.difftime(round(maxtime/100), units="hours")+ as.difftime(maxtime%%100, units="mins"), "%r",tz="UTC") 
```

```
## [1] "08:35:00 AM"
```

## Imputing missing values

The total number of missing values (NAs) in the dataset:


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

I use the mean for 5-minute interval as replacement for the missing values.


```r
fixed<-activity
fixed[is.na(fixed[, 1]), 1]<-avg_steps[is.na(fixed[, 1]),2]
```

Histogram of the new dataset, which has the NAs replaced with the 5-minute means.


```r
qplot(date, weight=fixed$steps, data=fixed, geom="histogram")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Calculate and report the mean and media values. We can see that the values differ from the initial calculation, and have higher value. It is also observed in the graph above. Most days have an increased number of mean of steps, since the NAs have been replaced by the 5-minute interval mean.


```r
mean(tapply(fixed$steps, fixed$date, sum, na.rm = TRUE))
```

```
## [1] 10766.19
```

```r
median(tapply(fixed$steps, fixed$date, sum, na.rm = TRUE))
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Append "Weekend" or "Weekday" to a new field, conditionally.


```r
library(lattice)
fixed$day<-as.factor(ifelse(weekdays(fixed$date) %in% c("Saturday","Sunday"),"Weekend","Weekday"))
```

Plot the 5-minute average of steps, by weekday/weekend.


```r
xyplot(steps ~ interval | day, aggregate(steps ~ interval + day, fixed, FUN = mean), layout = c(1, 2), type = "l", group=day)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
