---
title: "Reproducible Research Assignment -1"
output: 
  html_document: 
    keep_md: yes
---


```r
    library(knitr)
    library(dplyr)
    library(ggplot2)
```
## Download data set & Reading the data frame


```r
    fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileURL,destfile="./dataset.zip",method="curl")
    unzip(zipfile = "./dataset.zip")
    activity <- read.csv("./activity.csv",stringsAsFactors = FALSE)
    activity$date <- as.Date(activity$date)
```

## Histogram of Total steps per day


```r
    TotalStepsPerDay <- with(activity,tapply(steps, date, sum))
    hist(TotalStepsPerDay, breaks = 50,col="steelblue",xlab = "Total Steps per Day",
            ylab ="Frequency", main =" Histogram: Total Steps Per Day")
```

![](PA1_template_files/figure-html/Histogram plot -1.png)<!-- -->

##Mean of steps taken each day


```r
    mean(TotalStepsPerDay,na.rm =TRUE)
```

```
## [1] 10766.19
```

##Median of steps taken each day

```r
    median(TotalStepsPerDay,na.rm =TRUE)
```

```
## [1] 10765
```

##Time series plot of the average number of steps taken

```r
    steps_by_interval <- aggregate(steps ~ interval, activity, mean)

    g  <- ggplot(steps_by_interval, aes(interval, steps), colour = 'red') + geom_line()  
    g + xlab("5-minute interval") + ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/plot-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps
 - Time Series that contain activity of maximum steps 

```r
    max_steps<- max(steps_by_interval$steps)
    yy <- which(grepl(max_steps, steps_by_interval$steps))
    steps_by_interval[yy,]
```

```
##     interval    steps
## 104      835 206.1698
```

 - Average no of steps  

```r
    avg_steps <- mean(steps_by_interval$steps)
    avg_steps
```

```
## [1] 37.3826
```
- Total no of Time Series that contain steps above average

```r
    sum(steps_by_interval$steps>=avg_steps )
```

```
## [1] 136
```


## Code to describe and show a strategy for imputing missing data


```r
    No_activity<-is.na(activity$steps)
    sum(No_activity)
```

```
## [1] 2304
```



```r
for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- steps_by_interval$steps[which(steps_by_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}

# Aggregate the steps per day with the imputed values
impute_steps_per_day <- aggregate(steps ~ date, activity, sum)

# Draw a histogram of the value 
hist(impute_steps_per_day$steps, main = "Histogram of total number of steps per day (Imputed)", xlab = "Steps per day")
```

![](PA1_template_files/figure-html/impute-1.png)<!-- -->

##Are there differences in activity patterns between weekdays and weekends?
- Subseting the data set by weekdays and weekends
- Convering the day type column to a factor column
- Aggregate the Weekday and weekend activity 


```r
    activity['day_type'] <-weekdays(activity$date)
    activity$day_type[activity$day_type  %in% c('Saturday','Sunday') ] <- "weekend"
    activity$day_type[activity$day_type != "weekend"  ] <- "weekday" 
    activity$day_type <- as.factor(activity$day_type)
    stepsMeanPerWeekday <- aggregate(steps ~ interval + day_type, activity, mean)
```
- Plot of activity patterns between weekdays and weekends?


```r
# Display the 2 plots
    qplot(interval,  steps, data = stepsMeanPerWeekday, geom=c("line"), xlab = "Interval", 
          ylab ="Number of steps", color = day_type) +facet_wrap(~ day_type, ncol = 1)
```

![](PA1_template_files/figure-html/ploting the ggraph-1.png)<!-- -->

    
