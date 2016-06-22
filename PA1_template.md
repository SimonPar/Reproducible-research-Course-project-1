Reproducible Research: Course Project 1
=============================================

## First, let's see what is our directory, set our directory, etc.

```r
getwd()
```

```
## [1] "/Users/Simon/Coursera_R_programming/Course project 1 - Reproducible research"
```

```r
setwd("/Users/Simon/Coursera_R_programming/Course project 1 - Reproducible research")
```

## Second, let's load the libraries that we will use

```r
library(ggplot2)
library(scales)
library(Hmisc)
```

## Third, let's load the dataset to be analysed
### 1. Load the dataset using read.csv()

```r
if(!file.exists("activity.csv")) {
        unzip("activity.csv")
}
activity.data <- read.csv("activity.csv")
```

### 2. Before any data analysis, let's see how the data look like

```r
head(activity.data)
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
summary(activity.data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## We begin with the data analysis

```r
steps.per.day <- aggregate(steps ~ date, activity.data, sum)
```

## What is the mean total number of steps taken per day?
### 1. Create histogram of total number of steps taken per day

```r
hist(steps.per.day$steps, main = "Total steps per day", col = "royalblue", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

### 2. Compute mean and median of steps taken per day

```r
mean.steps <- mean(steps.per.day$steps)
median.steps <- median(steps.per.day$steps)

mean.steps
```

```
## [1] 10766.19
```

```r
median.steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### 1. Calculate average steps for each interval for all days

```r
steps.per.interval <- aggregate(steps ~ interval, activity.data, mean)
```

### 2. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(steps.per.interval$interval, steps.per.interval$steps, type="l", xlab = "Interval", ylab = "Number of Steps",main = "Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

### 3. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.interval <- steps.per.interval[which.max(steps.per.interval$steps), 1]
```

## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
total.na <- sum(!complete.cases(activity.data))
```

### 2. Devise a strategy for filling in all of the missing values in the dataaset
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
activity.data.imputed <- activity.data
activity.data.imputed$steps <- impute(activity.data$steps, fun = mean)
```

### 4. Create a histogram of the total number of steps taken each day (imputed dataset)

```r
steps.per.day.imputed <- aggregate(steps ~ date, activity.data.imputed, sum)
hist(steps.per.day.imputed$steps, main = "Total steps per day (imputed)", col = "red", xlab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

### 5. Compute new mean and median for imputed dataset

```r
mean.steps.imputed <- mean(steps.per.day.imputed$steps)
median.steps.imputed <- median(steps.per.day.imputed$steps)

mean.steps.imputed
```

```
## [1] 10766.19
```

```r
median.steps.imputed
```

```
## [1] 10766.19
```

### 6. Calculate difference between original and imputed datasets

```r
mean.diff <- mean.steps.imputed - mean.steps
median.diff <- median.steps.imputed - median.steps

mean.diff
```

```
## [1] 0
```

```r
median.diff
```

```
## [1] 1.188679
```

### 7. Calculate total difference

```r
total.diff <- sum(steps.per.day.imputed$steps) - sum(steps.per.day$steps)

total.diff
```

```
## [1] 86129.51
```

## Are there differences in acticuty patterns between weekdays and weekends?
### 1. Create a new factor variable in the dataset with two levels, "weekday" and "weekend", indicating whether a given date is a weekday or weekend day.

```r
activity.data.imputed$date.type <- ifelse(as.POSIXlt(activity.data.imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
averaged.activity.data.imputed <- aggregate(steps ~ interval + date.type, data = activity.data.imputed, mean)

ggplot(averaged.activity.data.imputed, aes(interval, steps)) +
        geom_line() + 
        facet_grid(date.type ~ .) + 
        xlab("5-minute interval") + 
        ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

