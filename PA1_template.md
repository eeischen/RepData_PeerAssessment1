# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# download and unzip the data file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
    "activity.zip", method = "curl")
a <- unz("activity.zip", "activity.csv")
activity <- read.csv(a)
```



## What is mean total number of steps taken per day?

Here is a histogram of the total number of steps taken each day.

```r
# make a vector with the total number of steps for each day
dates <- levels(factor(activity$date))
length <- length(dates)
totalsteps <- rep(NA, length)
for (i in 1:length) {
    temp <- activity[as.character(activity$date) == dates[i], "steps"]
    totalsteps[i] <- sum(temp, na.rm = TRUE)
}

# make a histogram of total steps per day
hist(totalsteps, main = "Histogram of Total Steps per Day", xlab = "Total Steps per Day")
```

![plot of chunk stepsperday](figure/stepsperday.png) 


Here is the mean number of steps taken per day.


```r
mean(totalsteps)
```

```
## [1] 9354
```


 Here is the median number of steps taken per day.


```r
median(totalsteps)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

Here is a time series plot of the 5-minute intervals and the average number of steps during each interval (averaged across all days).

```r

# make a vector with the average number of steps per interval
intervals <- levels(factor(activity$interval))
lengthint <- length(intervals)
avgsteps <- rep(NA, lengthint)
for (i in 1:lengthint) {
    stepsint <- activity[as.character(activity$interval) == intervals[i], "steps"]
    avgsteps[i] <- mean(stepsint, na.rm = TRUE)
}

# make a time series plot of average number of steps per interval
plot(avgsteps ~ intervals, type = "l", xlab = "5-minute interval", ylab = "average number of steps taken")
```

![plot of chunk avgactivitypattern](figure/avgactivitypattern.png) 


Here is the five-minute interval that, on average across all the days in the data set, contains the maximum number of steps:

```r
intervals[which.max(avgsteps)]
```

```
## [1] "835"
```


## Imputing missing values
The total number of missing values in the dataset:

```r
step <- activity$steps
length(step[is.na(step)])
```

```
## [1] 2304
```


### A strategy for filling in missing values
Create a new dataset, activitynew, as follows.  For each 5-minute interval that is missing data, replace each "NA" from the original file with the mean for that 5-minute interval.


```r

# make a new data frame with missing entries filled in with averages for
# corresp. interval
activitynew <- activity
for (i in 1:nrow(activity)) {
    if (is.na(activity[i, "steps"])) {
        j <- activity[i, "interval"]
        avg <- avgsteps[which(intervals == j)]
        activitynew[i, "steps"] <- avg
    }
}
```


Here is a histogram of the total number of steps taken each day.


```r

# make vector with total number of steps for each day
totalstepsnew <- rep(NA, length)
for (i in 1:length) {
    temp <- activitynew[as.character(activitynew$date) == dates[i], "steps"]
    totalstepsnew[i] <- sum(temp)
}

# make histogram for total number of steps for each day
hist(totalstepsnew, main = "Histogram of Total Steps per Day", xlab = "Total Steps per Day")
```

![plot of chunk totalnew](figure/totalnew.png) 


Here is the mean total number of steps taken per day.


```r
mean(totalstepsnew)
```

```
## [1] 10766
```


Here is the median total number of steps taken per day.


```r
median(totalstepsnew)
```

```
## [1] 10766
```


These values are higher than the estimates from the first part of the assignment.  This is because the total now includes all the steps from before, summed together with estimates for the previously disregarded intervals.

## Are there differences in activity patterns between weekdays and weekends?

This code creates a new factor variable in the data set with two levels: "weekday" and "weekend", indicating whether each date is a weekday or a weekend day.


```r

# make function that determines whether weekday or weekend day
partofweek <- function(date) {
    whichweekday <- weekdays(as.Date(date))
    if (whichweekday == "Saturday" || whichweekday == "Sunday") {
        "weekend"
    } else {
        "weekday"
    }
}

# make dataframe activitynewdays with factor variable that says whether
# weekend or weekday
partofweekcol <- as.factor(sapply(activitynew$date, partofweek))
activitynewdays <- cbind(activitynew, partofweekcol)
names(activitynewdays)[4] <- "Part of week"
```


Here is a time series plot of the five-minute intervals and the average number of steps taken, averaged across all weekday days or weekend days.  (We are now using the data set activitynew, with estimates for the missing values filled in.)


```r

# make vector of average steps per interval for weekend days
wkendavg <- rep(NA, lengthint)
for (i in 1:lengthint) {
    stepsint <- activitynewdays[as.character(activitynewdays$interval) == intervals[i] & 
        activitynewdays$"Part of week" == "weekend", "steps"]
    wkendavg[i] <- mean(stepsint)
}

# make vector of average steps per interval for weekdays
wkdayavg <- rep(NA, lengthint)
for (i in 1:lengthint) {
    stepsint <- activitynewdays[as.character(activitynewdays$interval) == intervals[i] & 
        activitynewdays$"Part of week" == "weekday", "steps"]
    wkdayavg[i] <- mean(stepsint)
}

# make time series plot of the five-minute intervals and the average number
# of steps taken, averaged across all weekday days or weekend days
avgdf <- data.frame(c(rep("weekday", lengthint), rep("weekend", lengthint)), 
    c(wkdayavg, wkendavg), c(as.numeric(intervals), as.numeric(intervals)))
names(avgdf) <- c("daytype", "averages", "intervals")
library(lattice)
xyplot(averages ~ intervals | daytype, data = avgdf, type = "l", xlab = "Interval", 
    ylab = "Number of steps", layout = c(1, 2))
```

![plot of chunk activitypatternnew](figure/activitypatternnew.png) 

