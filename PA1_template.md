# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Get the data into r's memory

```r
unzip('activity.zip')
activityData <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?

### 1. Make a histogram of the total number of steps taken each day

```r
# Creates a table which counts the total number of steps taken for each each day
totalStepsTakenPerDay <- aggregate(steps ~ date, data = activityData, FUN = 'sum', na.exclude = T)
# Make the histogram
hist(totalStepsTakenPerDay$steps, xlab="Steps per day", main="Total number of steps taken each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

### 2. Calculate and report the mean and median total number of steps taken per day


```r
meanOfTotalNumSteps <- mean(totalStepsTakenPerDay$steps)
medianOfTotalNumSteps <- median(totalStepsTakenPerDay$steps)
```

The mean of the total number of steps is 1.0767189\times 10^{4}.

The mean of the total number of steps is 10766.

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Creates a table of average number of steps for each time interval
avgStepsPerTimeInterval <- aggregate(steps ~ interval, data = activityData, FUN = 'mean', na.exclude = T)

# Make time series plot
plot (
    x = avgStepsPerTimeInterval$interval
  , y = avgStepsPerTimeInterval$steps 
  , xlab = 'Time interval of 5 mins'
  , ylab = 'Average number of steps per time interval from all days'
  , main = 'Time series plot'
  , type='l'
)
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
timeIntervalWithMostNumOfSteps <- avgStepsPerTimeInterval[which.max(avgStepsPerTimeInterval$steps), ]$interval 
```

The 5-minute interval, **835**, on average across all the days in the dataset, contains the most number of steps.

## Inputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numberOfRowsWithNas <- sum((is.na(activityData$steps) | is.na(activityData$date) | is.na(activityData$interval))) 
```

There are **2304** rows with missing values in the dataset.


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends?
