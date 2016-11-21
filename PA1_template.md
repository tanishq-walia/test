---
title: "RepR project1"
author: "Tikam Singh"
date: "19 November 2016"
output: html_document
keep_md: yes
---
# Loading Required packages
```{r,include=TRUE,fig.show='asis'}
library(dplyr)

library(ggplot2)
library(lubridate)
```


# Loading and preprocessing the data
```{r, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)

data<-read.csv("activity.csv")
data$date <- ymd(data$date)
```

# What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day


```{r,include=TRUE}
data.byday <- data %>% 
  group_by(date) %>%
  mutate(totalsteps = sum(steps, na.rm = T))
```

* Histogram of the total number of steps taken each day

```{r,include=TRUE,fig.show='asis'}
ggplot(data = data.byday, aes(x = totalsteps)) +
  geom_histogram(binwidth = 1000) +
  labs(title = "Histogram: number of steps by day",
       x = "Number of steps",
       y = "Frequency")



meansteps <- mean(data.byday$totalsteps, na.rm = T)
mediansteps <- median(data.byday$totalsteps, na.rm = T)
```

## *The average amount of steps per day is r meansteps, the median amount is r             mediansteps.*


---
+ What is the average daily data pattern?
+ Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and         the average number of steps taken, averaged across all days (y-axis)
---

```{r,include=TRUE,fig.show='asis'}
data.byinterval <- group_by(data, interval) %>% 
  mutate(meansteps = mean(steps, na.rm = T))

ggplot(data = data.byinterval, 
       aes(x = interval, y = meansteps)) +
  geom_line(na.rm = T) + 
  labs(title = "Average steps taken by interval",
       x = "interval (min)",
       y = "average amount of steps")
```


## *Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?*                


```{r,include=TRUE}
maxsteps <- data.byinterval[which.max(data.byinterval$meansteps), ]
```
---
The r maxsteps$interval minute interval contains the maximum number of steps 
(r maxsteps$meansteps) on average across all the days.
Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e.

the total number of rows with NAs)
---

```{r,include=TRUE}
complete <- sum(complete.cases(data))
missing <- nrow(data) - complete
```
---
There are r missing rows with missing values in the dataset.
Fill missing values in the dataset.
---

## Missing values have been replaced with the mean value for that interval.
```{r,include=TRUE}
imputed <- group_by(interval, .data = data) %>% 
  mutate(steps = ifelse(is.na(steps), as.integer(mean(steps, na.rm = T)), steps))
```
### Make a histogram of the total number of steps taken each day


```{r,include=TRUE,fig.show='asis'}
imputed.byday <- group_by(date, .data = imputed) %>%
  mutate(totalsteps = sum(steps))

ggplot(data = imputed.byday, aes(x = totalsteps),stat_bin(binwidth = 1000)) +
  geom_histogram() +
  labs(title = "Histogram: number of steps by day",
       x = "Number of steps",
       y = "Frequency")
```

### Calculate and report the mean and median total number of steps taken per day.
```{r,include=TRUE}
imputed.meansteps <- mean(data.byday$totalsteps)
imputed.mediansteps <- median(data.byday$totalsteps)
```
---
With the missing values filled, the average amount of steps per day is r imputed.meansteps, the median amount is r imputed.mediansteps.
In contrast when missing values are not taken into account the average amount of steps per day is r meansteps, the median amount is r mediansteps.

Are there differences in data patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
---
```{r,include=TRUE}
imputed <- imputed %>% 
  mutate(weekday = as.factor(
    ifelse(wday(date) %in% c(1,7), "weekend", "weekday"))) 
```
---
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
---


```{r,include=TRUE,fig.show='asis'}
imputed.byinterval <- group_by(interval, weekday, .data = imputed) %>% 
  mutate(meansteps = mean(steps, na.rm = T))

ggplot(data = imputed.byinterval, 
       aes(x = interval, y = meansteps)) +
  geom_line() + 
  facet_grid(weekday ~ .) +
  labs(title = "Average steps taken by interval",
       x = "interval (min)",
       y = "average amount of steps") 

```

