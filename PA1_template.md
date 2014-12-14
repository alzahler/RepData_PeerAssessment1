# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We use the read.csv function to load the data, that is in the same directory as the RMD file:

```r
data <- read.csv("activity.csv")
data1 <- na.omit(data)
str(data1)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
##   .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
```

Then we transform the $date and $interval variables into their appropiate class: date and factor, respectively.

```r
data1$date <- as.Date(data1$date, "%Y-%m-%d")
data1$interval <- as.factor(data1$interval)
str(data1)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-02" "2012-10-02" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:2304] 1 2 3 4 5 6 7 8 9 10 ...
##   .. ..- attr(*, "names")= chr [1:2304] "1" "2" "3" "4" ...
```


## What is mean total number of steps taken per day?

First, we make a histogram of the total number of steps taken each day

```r
library(ggplot2)
sday <- aggregate(steps ~ date, data = data1, sum)
qplot(steps, data=sday, geom="histogram")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-3](PA1_template_files/figure-html/unnamed-chunk-3.png) 

Secondly, we calculate and report the mean and median total number of steps taken per day.

```r
mean(sday$steps)
```

```
## [1] 10766
```

```r
median(sday$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

First, we calculate using the aggregate function, the average number of steps taken averaged across all days.

```r
a.day <- aggregate(steps ~ interval, data = data1, mean)
```

Then, we make a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
a.day$interval <- as.numeric(as.character(a.day$interval))
qplot(interval, steps, data=a.day, geom="line")
```

![plot of chunk unnamed-chunk-6](PA1_template_files/figure-html/unnamed-chunk-6.png) 

Finally, we calculate which 5-minute interval , on average, contains the maximum number of steps:

```r
max <- max(a.day$steps)
index <- as.logical(match(a.day$steps, max))
interval <- a.day$interval[which(index)]
interval
```

```
## [1] 835
```

## Imputing missing values

We calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

For the missing values (NA's), we will use the mean for that 5-minute interval across the month, which was calculated before as the dataframe a.day.

Next, we create the new dataset that is equal to the original dataset but with the missing data filled in. For this, we'll cycle the data set and check if there is NA. If there is, we'll replace the NA with the mean that corresponds to that interval.


```r
data2 <- data
nas <- !complete.cases(data2)

for (i in 1:dim(data2)[1]){
        if (nas[i] == TRUE) {
                mean <- a.day[which(a.day$interval == data2[i,3]),2]
                data2[i,1] = mean
        }
}

data2$date <- as.Date(data2$date, "%Y-%m-%d")

#no NAs :)
sum(!complete.cases(data2))
```

```
## [1] 0
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
s2day <- aggregate(steps ~ date, data = data2, sum)
qplot(steps, data=s2day, geom="histogram")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-10](PA1_template_files/figure-html/unnamed-chunk-10.png) 

Secondly, we calculate and report the mean and median total number of steps taken per day.

```r
mean(s2day$steps)
```

```
## [1] 10766
```

```r
median(s2day$steps)
```

```
## [1] 10766
```

The mean stays the same, but the median changes slightly. However, the total daily number of steps, there is an increase of 86,129 steps, which corresponde to 15,1%.

```r
total1 <- sum(sday$steps); total1
```

```
## [1] 570608
```

```r
total2 <- sum(s2day$steps); total2
```

```
## [1] 656738
```

```r
total2-total1
```

```
## [1] 86130
```

## Are there differences in activity patterns between weekdays and weekends?

We create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
for (i in 1:dim(data2)[1]){
        day <- weekdays(data2[i,2])

        if ((day == "Saturday") | (day =="Sunday")){
                data2[i,4] = "weekend"
        }
        else {
                data2[i,4] = "weekday"
        }
}
data2[,4] <- as.factor(data2[,4])
str(data2)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ V4      : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Finally, we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
a2.day <- aggregate(steps ~ interval + V4, data = data2, FUN="mean")
head(a2.day)
```

```
##   interval      V4   steps
## 1        0 weekday 2.25115
## 2        5 weekday 0.44528
## 3       10 weekday 0.17317
## 4       15 weekday 0.19790
## 5       20 weekday 0.09895
## 6       25 weekday 1.59036
```

```r
qplot(interval, steps, data=a2.day, facets = V4 ~ ., geom="line")
```

![plot of chunk unnamed-chunk-14](PA1_template_files/figure-html/unnamed-chunk-14.png) 
