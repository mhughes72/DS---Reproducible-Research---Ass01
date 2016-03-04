# Reproducible Research: Peer Assessment 1


## Loading the data


```r
  data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
  total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
  library(ggplot2)
  qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![](reproducible_files/figure-html/unnamed-chunk-2-1.png)

```r
  mean(total.steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
  median(total.steps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
  library(ggplot2)
  averages <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), 
                        FUN = mean, na.rm = TRUE)
  ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + 
    ylab("average number of steps taken")
```

![](reproducible_files/figure-html/unnamed-chunk-3-1.png)

```r
    averages[which.max(averages$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
  #Replace NA data with the average of all Steps
  data2 <- data
  data2$steps[which(is.na(data2$steps))] <- mean(data2$steps, na.rm=TRUE)
  head(data2, n=20)
```

```
##      steps       date interval
## 1  37.3826 2012-10-01        0
## 2  37.3826 2012-10-01        5
## 3  37.3826 2012-10-01       10
## 4  37.3826 2012-10-01       15
## 5  37.3826 2012-10-01       20
## 6  37.3826 2012-10-01       25
## 7  37.3826 2012-10-01       30
## 8  37.3826 2012-10-01       35
## 9  37.3826 2012-10-01       40
## 10 37.3826 2012-10-01       45
## 11 37.3826 2012-10-01       50
## 12 37.3826 2012-10-01       55
## 13 37.3826 2012-10-01      100
## 14 37.3826 2012-10-01      105
## 15 37.3826 2012-10-01      110
## 16 37.3826 2012-10-01      115
## 17 37.3826 2012-10-01      120
## 18 37.3826 2012-10-01      125
## 19 37.3826 2012-10-01      130
## 20 37.3826 2012-10-01      135
```
## Are there differences in activity patterns between weekdays and weekends?

  
  
