---
title: "Exploratory Data Analysis Assignment- Course Project 1"
author: "Husm"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: html_document
---


## R Markdown

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:  
**Dataset:** [URL](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

```{r  Loading_libraries,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
library(dplyr)
library(lubridate)
library(lattice)
```

```{r  Loading_and_preprocessing_data,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- tbl_df(read.csv(unz(temp, "activity.csv")))
unlink(temp)
data$date <- ymd(data$date)
```
## What is mean total number of steps taken per day?
```{r   total_number_of_steps_taken_per_day,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
Steps_Per_Day <- aggregate(steps~date,data=data,sum,na.rm=TRUE)
hist(Steps_Per_Day$steps,breaks =100, xlab = "Total number of steps per day", col=4, main=" Total number of steps taken each day")
mean_of_steps <- round(mean(Steps_Per_Day$steps))
mean_of_steps
median_of_steps <-  median(Steps_Per_Day$steps)
options("scipen" = 10)
options()$scipen
```

**The mean of total number of steps per day is ```r mean_of_steps```**
**The median of total number of steps per day is ```r median_of_steps```**


## What is the average daily activity pattern?
```{r   average_daily_activity_pattern,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
daily_activity_pattern<- tapply(data$steps,data$interval,mean,na.rm=TRUE)

plot(row.names(daily_activity_pattern), daily_activity_pattern, type = "l", col = "2",
     xlab = "5 min interval",  ylab = "Average steps per day averaged",
     main = "Average number of steps per day")
```

```{r max_interval,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
max_interval <- which.max(daily_activity_pattern)
```

**The maximum 5-min interval that contains the maximum number of steps is `r names(max_interval)`**

## Imputing missing values

```{r Count_of_missing_value,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
count_of_missing_values <- sum(is.na(data))
```
**The total number of missing values is `r count_of_missing_values`**

```{r filling_missing_value,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
Average_Steps <- aggregate(steps ~ interval, data= data, FUN = mean)
MissingValueFilling <- numeric()
for (i in 1:nrow(data)) {
    data2 <- data[i, ]
    if (is.na(data2$steps)){
        steps <- subset(Average_Steps, interval == data2$interval)$steps
    } else{
        steps <- data2$steps
    }
    MissingValueFilling <- c(MissingValueFilling, steps)
}
```

```{r new_data_set_with_no_NAs,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
new_data_fill <- data
new_data_fill$steps <- MissingValueFilling
count_of_NAs <- sum(is.na(new_data_fill))
```
**The total number of NAs in the new data set is ```r count_of_NAs```**

```{r histogram_mean_median_for_new_data set,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
Steps_Per_Day2 <- aggregate(steps~date,data=new_data_fill,sum,na.rm=TRUE)
hist(Steps_Per_Day2$steps,breaks =100, xlab = "Total number of steps per day", col=4, main = "Total number of steps taken each day ")

new_data_mean <- round(mean(Steps_Per_Day2$steps))
new_data_median <- round(median(Steps_Per_Day2$steps))

```

**The mean for the new data set is ```r new_data_mean``` which is equal to the old data mean ```r mean_of_steps```**
**The mean for the new data set is ```r new_data_median``` which is greater than the old data median ```r median_of_steps```**

## Differences in activity patterns between weekdays and weekends

```{r  total_number_of_steps_taken_per_day_for_new_data_set,  echo = TRUE, error=FALSE, message=FALSE, warning=FALSE}
new_data_fill$day <- factor(format(new_data_fill$date, "%A"))
levels(new_data_fill$day) <- list(weekday = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday"),
                                  weekend = c("Saturday", "Sunday"))
AvgStepsAcrossDays <- aggregate(new_data_fill$steps,
                                list(interval = as.numeric(as.character(new_data_fill$interval)),
                                day = new_data_fill$day), FUN=mean)
names(AvgStepsAcrossDays)[3] <- "steps"
xyplot(AvgStepsAcrossDays$steps ~ AvgStepsAcrossDays$interval | AvgStepsAcrossDays$day, 
       layout =c(1,2), type="l",xlab="interval", ylab="Avg no. of steps")
```


