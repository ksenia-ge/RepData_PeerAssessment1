# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Firstly, unzip compressed file to generate csv file, load data into R and save as object activity_data. Look at loaded data using str(), to check format, variables and number of observations

```r
unzip("activity.zip")
activity_data <- read.csv("activity.csv")
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
The dataframe consists of 3 variables and 17568 observations, including NA values for the steps variable; the date variable has 61 levels (61 days).

Write each variable of activity_data as a separate vector, and creates a vector of unique date values.

```r
steps <- activity_data$steps
dates <- activity_data$date
ids <- activity_data$interval

unique_dates <- unique(dates)
```


Then create blank numeric vectors that will contain daily values of steps, the number of non-NA observations and the number of NAs.


```r
dailySteps <- vector(mode = "numeric", length = length(unique_dates))
numberDailyObs <- vector(mode = "numeric", length = length(unique_dates))
numberDailyNAs <- vector(mode = "numeric", length = length(unique_dates))
```

Then for each day, sum the number of steps, NA and non-NA values.

```r
for (i in seq_along(unique_dates)) {
        logicDay <- dates == unique_dates[i]
        daySubset <- steps[logicDay]
        dailySteps[i] <- sum(daySubset, na.rm = TRUE)
        numberDailyNAs[i] <- sum(is.na(daySubset))
        numberDailyObs[i] <- sum(!is.na(daySubset))
}
```
Then plot a histogram of daily steps, and calculate and report mean and median number of daily steps


```r
hist(dailySteps, breaks = 8)
```

![](PA1_complete_files/figure-html/unnamed-chunk-5-1.png) 

```r
meanSteps <- mean(dailySteps)
medianSteps <- median(dailySteps)
paste("mean daily steps =", meanSteps, sep = " ")
```

```
## [1] "mean daily steps = 9354.22950819672"
```

```r
paste("median daily steps =", medianSteps, sep = " ")
```

```
## [1] "median daily steps = 10395"
```

## What is the average daily activity pattern?

First, create vector of unique interval ids, and blank vectors to take the mean number of steps for each interval averaged over the days, and the number of non-NA and NA observations for each interval.


```r
unique_id <- unique(ids)
idSteps <- vector(mode = "numeric", length = length(unique_id))
idObs <- vector(mode = "numeric", length = length(unique_id))
idNAs <- vector(mode = "numeric", length = length(unique_id))
```

Then for each interval, take the mean number of steps over all days, and similarly for NA and non-NA values.

```r
for (i in seq_along(unique_id)) {
        logicId <- ids == unique_id[i]
        idSubset <- steps[logicId]
        idSteps[i] <- mean(idSubset, na.rm = TRUE)
        idNAs[i] <- sum(is.na(idSubset))
        idObs[i] <- sum(!is.na(idSubset))
}
```
Now create a dataframe with unique interval id and mean step values and make the time series plot.


```r
intervalMean <- cbind(unique_id, idSteps)
tsId <- ts(intervalMean)
plot(tsId)
```

![](PA1_complete_files/figure-html/unnamed-chunk-8-1.png) 

Then find and report the interval id which has maximum mean steps.

```r
peakId <- unique_id[idSteps == max(idSteps)]
paste("Interval with maximum number of steps averaged over all days is", peakId, sep = " ")
```

```
## [1] "Interval with maximum number of steps averaged over all days is 835"
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
