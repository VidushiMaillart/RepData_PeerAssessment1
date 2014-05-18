# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data


```r
data <- read.csv("./activity.csv")
```



## What is mean total number of steps taken per day?


```r

s <- split(data, data$date)

# Answer (1)
steps_per_day <- sapply(s, function(x) sum(x["steps"], na.rm = TRUE))
hist(steps_per_day, xlab = "total number of steps per day", main = "Histogram of the number steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

# Answer (2)
mean(steps_per_day)
```

```
## [1] 9354
```

```r
median(steps_per_day)
```

```
## [1] 10395
```



## What is the average daily activity pattern?


```r
# Answer (1)

s <- split(data, data$interval)
av_steps <- sapply(s, function(x) mean(x[, "steps"], na.rm = TRUE))

x <- seq(0, 288, 1)
x1 <- seq(0, 1440, 5)
time = NULL
for (i in 1:288) {
    time[i] = sprintf("%02d:%02d", x[i]%/%12, x1[i]%%60)
}
time <- as.factor(time)
par(mar = c(7, 4, 4, 2) + 0.3)
plot(av_steps, type = "l", xaxt = "n", xlab = "5-minute time intervals", ylab = "average number of steps")
axis(1, at = seq(1, 288, 10), labels = FALSE)
labels <- paste(time[seq(1, 288, 10)], sep = " ")
text(seq(1, 288, 10), par("usr")[1] - 3, srt = 90, adj = 1, labels = labels, 
    xpd = TRUE)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



```r
# Answer (2)
av <- data.frame(time, av_steps)
m <- max(av_steps)
av[(av_steps == m), ]
```

```
##      time av_steps
## 835 08:35    206.2
```



## Imputing missing values


```r
# Answer (1)

# Only column steps misses values, the other two are complete
ok <- complete.cases(data$date, data$interval)
sum(!ok)
```

```
## [1] 0
```

```r

# Number of rows with missing values is
ok <- complete.cases(data$steps)
missing <- sum(!ok)
missing
```

```
## [1] 2304
```





```r
# Answer (2) and Answer (3): using mean for that 5-minute interval
data_new <- data
data_new$av_steps <- av_steps
data_new$steps[is.na(data_new$steps)] <- data_new$av[is.na(data_new$steps)]
```





```r
# Answer 4

s <- split(data_new, data_new$date)
steps_per_day_new <- sapply(s, function(x) sum(x["steps"], na.rm = TRUE))
hist(steps_per_day_new, xlab = "total number of steps per day", main = "Histogram of the number steps per day with new data")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r

mean(steps_per_day_new)
```

```
## [1] 10766
```

```r
median(steps_per_day_new)
```

```
## [1] 10766
```

```r

# For comparison
mean(steps_per_day)
```

```
## [1] 9354
```

```r
median(steps_per_day)
```

```
## [1] 10395
```

###Questions: 
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

####Answers:
Yes, these values differ as seen above. We get a much nicer Gaussian kind of distribution - the mean and the median fall together. But our data are now somewhat biased. 

## Are there differences in activity patterns between weekdays and weekends?
