# Reproducible Research: Peer Assessment 1

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
## Loading and preprocessing the data

data <- read.csv("G:\\Desktop\\Downloads\\RepData_PeerAssessment1-master\\RepData_PeerAssessment1-master\\activity.csv",colClasses = c("numeric", "Date", "numeric"))

## What is mean total number of steps taken per day?
byDay <- aggregate(steps ~ date, data, sum, na.action = na.pass)
byDay <- cbind(byDay, label = rep("with.na", nrow(byDay)))
ggplot(byDay, aes(x = steps)) + geom_histogram(binwidth = 1500, colour = "blue", 
    fill = "white") + labs(title = "Steps Taken per Day", x = "Number of Steps", 
    y = "Frequency")
```

![](./PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
## What is the average daily activity pattern?
byInterval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
ggplot(byInterval, aes(x = interval, y = steps)) + geom_line() + labs(title = "Average of Steps taken Daily", 
    x = "Interval", y = "Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-1-2.png) 

```r
## Imputing missing values
data.impute <- adply(data, 1, function(x) if (is.na(x$steps)) {
    x$steps = round(byInterval[byInterval$interval == x$interval, 2])
    x
} else {
    x
})
byDay.impute <- aggregate(steps ~ date, data.impute, sum)
byDay.impute <- cbind(byDay.impute, label = rep("without.na", nrow(byDay.impute)))
ggplot(byDay.impute, aes(x = steps)) + geom_histogram(binwidth = 1500, colour = "black", 
    fill = "white") + labs(title = "Steps Taken per Day", x = "Number of Steps", 
    y = "Frequency")
```

![](./PA1_template_files/figure-html/unnamed-chunk-1-3.png) 

```r
## Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale(locale = "C")
```

```
## [1] "C"
```

```r
data.weekend <- subset(data.impute, weekdays(date) %in% c("Saturday", "Sunday"))
data.weekday <- subset(data.impute, !weekdays(date) %in% c("Saturday", "Sunday"))

# Obtain the average steps per interval for each dataset
data.weekend <- aggregate(steps ~ interval, data.weekend, mean)
data.weekday <- aggregate(steps ~ interval, data.weekday, mean)

# By plotting we add a label
data.weekend <- cbind(data.weekend, day = rep("weekend"))
data.weekday <- cbind(data.weekday, day = rep("weekday"))
# Combine the subsets and a specify the levels
data.week <- rbind(data.weekend, data.weekday)
levels(data.week$day) <- c("Weekend", "Weekday")

ggplot(data.week, aes(x = interval, y = steps)) + geom_line() + facet_grid(day ~ 
    .) + labs(x = "Interval", y = "Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-1-4.png) 