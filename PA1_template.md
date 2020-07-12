

# Reproducible Research: Peer Assessment 1

---

title: "Reproducible Research: Peer Assessment 1"
output:
        html document:
                keep_md: TRUE

---

Here is the code for reading in and processing the data


```r
library(dplyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(ggplot2)

activdata <-read.csv("activity.csv")
activdata <- tbl_df(activdata)
activdata$dayname <- wday(activdata$date, label = TRUE)

activdata$day <- day(activdata$date)

activdata$month <- month(activdata$date)
```

Now plotting the total number of steps per day and calculating the mean and median number of total number of steps per day


```r
stepsday <- tapply(activdata$steps, activdata$date, sum, na.rm = TRUE)
qplot( x = stepsday, geom = "histogram") + geom_histogram(color = "white") + theme_bw() +
        labs(x = "Steps taken per day", y = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk Mean total number of steps taken per day](figure/Mean total number of steps taken per day-1.png)

```r
meansteps <- mean(activdata$steps, na.rm = T)
mediansteps <- median(activdata$steps, na.rm = T)
print("Mean total number of steps per day:")
```

```
## [1] "Mean total number of steps per day:"
```

```r
meansteps
```

```
## [1] 37.3826
```

```r
print("Median total number of steps per day:")
```

```
## [1] "Median total number of steps per day:"
```

```r
mediansteps
```

```
## [1] 0
```

Writing code to plot average number of steps for all intervals,averaged across all days and
printing the interval with maximum average number of steps


```r
intervalav <- tapply(activdata$steps, activdata$interval, mean, na.rm = TRUE)
intervalav <- cbind(rownames(intervalav), intervalav)
intervalav <- tbl_df(intervalav)
```

```
## Error: Column 1 must be named.
```

```r
colnames(intervalav) <- c("interval", "average")
intervalav <- tbl_df(intervalav)
intervalav$interval <- as.numeric(intervalav$interval)
intervalav$average <- as.numeric(intervalav$average)
qplot(data = intervalav, x = interval, y = average, geom = c("point","line")) +
        theme_bw() + labs(title = "Time series plot of average steps per interval of time", x = "Intervals",
                          y = "Average number of steps") 
```

![plot of chunk Relationship between intervals and number of steps taken](figure/Relationship between intervals and number of steps taken-1.png)

```r
maxinterval <- filter(intervalav, average == max(average))
print("Interval with maximum average number of steps is: ")
```

```
## [1] "Interval with maximum average number of steps is: "
```

```r
maxinterval$interval
```

```
## [1] 835
```

```r
print("The maximun average number of steps are: ")
```

```
## [1] "The maximun average number of steps are: "
```

```r
maxinterval$average
```

```
## [1] 206.1698
```

Writing code for reporting NA's, filling in the NA values and drawing means and medians again


```r
summary(activdata)
```

```
##      steps            date              interval      dayname         day            month      
##  Min.   :  0.00   Length:17568       Min.   :   0.0   Sun:2304   Min.   : 1.00   Min.   :10.00  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8   Mon:2592   1st Qu.: 8.00   1st Qu.:10.00  
##  Median :  0.00   Mode  :character   Median :1177.5   Tue:2592   Median :16.00   Median :10.00  
##  Mean   : 37.38                      Mean   :1177.5   Wed:2592   Mean   :15.75   Mean   :10.49  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2   Thu:2592   3rd Qu.:23.00   3rd Qu.:11.00  
##  Max.   :806.00                      Max.   :2355.0   Fri:2592   Max.   :31.00   Max.   :11.00  
##  NA's   :2304                                         Sat:2304
```

```r
print("Looking at the summary we conclude that the data has 2304 rows with NA values")
```

```
## [1] "Looking at the summary we conclude that the data has 2304 rows with NA values"
```

```r
# now replacing the NA values 

upactivdata <- activdata

intervalav <- tapply(upactivdata$steps, upactivdata$interval, mean, na.rm = TRUE)

nachecker <- is.na(activdata$steps)


for(i in 1:length(upactivdata$steps)){
        
        if (nachecker[i]==TRUE){
                
                upactivdata$steps[i] <- intervalav[as.character(activdata$interval[i])]
                
        }
}



stepsday <- tapply(upactivdata$steps, upactivdata$date, sum, na.rm = TRUE)
qplot( x = stepsday, geom = "histogram") + geom_histogram(color = "white") + theme_bw() +
        labs(x = "Steps taken per day", y = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk Imputing Missing Values](figure/Imputing Missing Values-1.png)

```r
meansteps <- mean(upactivdata$steps)
mediansteps <- median(upactivdata$steps)
print("Mean total number of steps per day:")
```

```
## [1] "Mean total number of steps per day:"
```

```r
meansteps
```

```
## [1] 37.3826
```

```r
print("Median total number of steps per day:")
```

```
## [1] "Median total number of steps per day:"
```

```r
mediansteps
```

```
## [1] 0
```

Finally making the plots for weekends and weekdays 


```r
upactivdata$daytype <- factor(upactivdata$dayname %in% c("Sat", "Sun"), labels = c("weekday","weekend"))
weekdata <- tapply(upactivdata$steps, list(upactivdata$interval, upactivdata$daytype), mean, na.rm = TRUE)
weekdata <- cbind(rownames(weekdata), weekdata[,1], weekdata[,2])
weekdata1$interval <- as.numeric(rownames(weekdata))
weekdata <- melt(weekdata1, id = "interval")
g <- ggplot(data = weekdata, aes(x = interval, y = value)) + facet_grid(variable~.) + theme_bw()
g + geom_point() + geom_line() + labs(x = "Intervals", y = "Average number of steps", title = "Average number of steps on weekends and weekdays")
```

![plot of chunk Weekend and Weekday plots with updated data](figure/Weekend and Weekday plots with updated data-1.png)
