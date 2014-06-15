Title
========================================================

This is an R Markdown document. Markdown is a simple formatting syntax for authoring web pages (click the **Help** toolbar button for more details on using R Markdown).

When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
#Loading and preprocessing the data
##Read file
Data <- read.csv("activity.csv", header=TRUE)
##Preprocess the data
##Data conversion
Data$date <- as.Date(Data$date) 
Data$steps <- as.numeric(Data$steps)
valid_steps <- subset(Data, steps>0,  select=c(date, steps))

#What is mean total number of steps taken per day?
#Question 1: Make a histogram of the total number of steps taken each day
##dates with valid steps are selected and 'date' is converted to character before being called at the loop.
total = 0
Date <- as.Date(unique(valid_steps$date))
seq_date <- as.character(Date)

##Processing the data to create histogram
date_hist<-character()
steps_per_day<-numeric()
for (i in seq_date){
        j <- as.Date(i)
        date_hist <- as.Date(date_hist)
        steps_day <-valid_steps[which(valid_steps$date ==j),]
        daily_steps <- steps_day$steps
        total <- sum(daily_steps)
        date_hist <- c(date_hist, j) 
        steps_per_day <- c(steps_per_day, total)
        
        
}

df <- data.frame(date_hist, steps_per_day )
hist(steps_per_day, breaks=30, col="lightgreen")

#Question 2: Calculate and report the mean and median total number of steps taken per day
##unique interval sets are derived by pre-processing
interval_list <- sort(unique(valid_steps$date))



#Average of steps for each interval is read and retrieved
steps_mean <-numeric()
steps_median <-numeric()
each_5min_interval <- numeric()
for (i in interval_list){
        group_steps <-valid_steps[which(valid_steps$date ==i),]
        group_steps$date <- sort(group_steps$date)
        mean_steps_eachday <- mean(group_steps$steps)
        median_steps_eachday <- median(group_steps$steps)
        steps_mean <- c(steps_mean, mean_steps_eachday)
        steps_median <- c(steps_median,median_steps_eachday )
        each_5min_interval <- c(each_5min_interval, i)
}

#Data frame created with 'each 5 min interval and average of steps pertaining to each averaged interval
df2 <- data.frame(each_5min_interval, steps_mean,steps_median )

#What is the average daily activity pattern?
#Question 1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
##Preprocess the data
Data$interval <- as.numeric(Data$interval)
Data$steps <- as.numeric(Data$steps)
valid_steps <- subset(Data, steps>0, (select=c(interval, steps)))


##unique interval sets are derived by pre-processing
interval_list <- sort(unique(valid_steps$interval))



#Average of steps for each interval is read and retrieved
steps_averaged <-numeric()
each_5min_interval <- numeric()
for (i in interval_list){
        group_steps <-valid_steps[which(valid_steps$interval ==i),]
        group_steps$interval <- sort(group_steps$interval)
        steps_at_each_interval <- mean(group_steps$steps)
        steps_averaged <- c(steps_averaged, steps_at_each_interval)
        each_5min_interval <- c(each_5min_interval, i)
}

#Data frame created with 'each 5 min interval and average of steps pertaining to each averaged interval
df <- data.frame(each_5min_interval, steps_averaged)


#Create a time series plot of the 5-minute interval(x-axis)  and the average number of steps taken, averaged across all days (y-axis)

library(ggplot2)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r
ggplot(df, aes(each_5min_interval, steps_averaged)) +geom_line() +
        scale_x_continuous(breaks=c(seq(0,2400,60)))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
#Question 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##Deriving the maximum steps from the average of steps
max_steps <- max(df2[,2])

#Imputing missing values
##Question 1: Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
MissingValues <- apply(Data,2,function(x) length(which(is.na(x))))
```

