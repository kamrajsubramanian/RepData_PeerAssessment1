---
title: "Workout Interpretations"
author: "Kamraj Subramanian"
date: "January 10, 2016"
output: html_document
---

reading the workout csv file after setting the working directory

```r
setwd("C:\\Users\\Kamraj\\Desktop\\temp")
activity <- read.csv("activity.csv",colClasses = c("numeric", "character","integer"))
```

Calling plyr and dplyr packages for preprocessing, lubridate for date class variables manipulation and ggplot2 for plotting the graph.

```r
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
```

Converting into date class

```r
activity$date <- ymd(activity$date)
```

1
Average number of steps taken everyday
total number of steps taken everyday 

```r
totalsteps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
hist(totalsteps,breaks=10, main = "Frequency of number of steps per day", 
    xlab = "number of steps taken everyday", ylab = "frequency", col = "blue")
```

![plot of chunk unnamed-chunk-4](figures/unnamed-chunk-4-1.png)

#mean and median of the total number of steps taken per day

```r
mean(totalsteps, na.rm = T)
```

```
## [1] 9354.23
```

```r
median(totalsteps, na.rm = T)
```

```
## [1] 10395
```

###2
##Average daily activity pattern
# time series plot of the 5-minute interval  and the average number of steps taken, averaged across all days:

```r
stepsaverageperinterval <- tapply(activity$steps, activity$interval, mean, na.rm = T)
plot(stepsaverageperinterval, type = "l", main = ("daily average over interval"), ylab = "average number of steps")
```

![plot of chunk unnamed-chunk-6](figures/unnamed-chunk-6-1.png)

#5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
seq(along = stepsaverageperinterval)[stepsaverageperinterval == max(stepsaverageperinterval)]
```

```
## [1] 104
```

###3
##Imputing missing values
#total number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

#filling in all of the missing values in the dataset with the mean by interval

```r
imputedactivity <- activity %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
```

#Make a histogram of the total number of steps taken each day

```r
stepstaken <- imputedactivity%>%
  group_by(date) %>%
  summarize(steps = sum(steps))

hist(stepstaken$steps, breaks = 5, main = "Frequency of number of steps per day",
    xlab = "Number of steps per day", ylab = "Frequency", col = "black")
```

![plot of chunk unnamed-chunk-10](figures/unnamed-chunk-10-1.png)

#As the mean is been used to impute the missing values, the plot looks crowded in the center.

```r
mean(stepstaken$steps)
```

```
## [1] 10766.19
```

```r
median(stepstaken$steps)
```

```
## [1] 10766.19
```
###4
##differences in activity patterns between weekdays and weekends
# factor variable with weekday, weekend

```r
tim <- as.POSIXlt(activity$date, format = "%Y-%m-%d")
WeekDay <- tim$wday
WeekDay[WeekDay == 0] = 0
WeekDay[WeekDay == 6] = 0
WeekDay[WeekDay != 0] = 1
WeekDayFactor <- factor(WeekDay, levels = c(0, 1))
activity$WeekDay <- WeekDayFactor
```

#  mean

```r
stepsaverageperweekday<- tapply(activity$steps, list(activity$interval, activity$WeekDay), mean, na.rm = T)

par(mfrow = c(2, 1))
with(activity, {
    par(mai = c(0, 1, 1, 0))
    plot(stepsaverageperweekday[, 1], type = "l", main = ("Number of steps across  interval"), 
        xaxt = "n", ylab = "Weekends")
    title = ("patterns across weekdays and weekends")
    par(mai = c(1, 1, 0, 0))
    plot(stepsaverageperweekday[, 2], type = "l", xlab = "Interval", ylab = "Week days")

})
```

![plot of chunk unnamed-chunk-13](figures/unnamed-chunk-13-1.png)
