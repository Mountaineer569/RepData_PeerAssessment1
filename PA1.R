# Reproducible Research - Project 1
### script: PA1.R
### created Oct-2015

# Load libraries.
library(plyr)

### Loading and preprocessing the data.
path <- "C:/Users/Robert/Documents/Coursera/R/Reproducible"

# After downloading and unzipping the data, we get these files.
list.files("/repdata_data_activity")

# 1. Load the data.
activity <- read.csv("C:/Users/Robert/Documents/Coursera/R/Reproducible/repdata_data_activity/activity.csv",
        header=TRUE, sep=",", stringsAsFactor=FALSE, na.strings="NA")

# 2. Format Date and Time fields.
activity$date <- strptime(activity$date, format = "%Y-%m-%d")

# Summarize the data to see what it contains.
summary(activity)

### What is mean total number of steps taken per day?
# 1. Calculate the total number of steps, mean and median of the total number of steps taken per day.
# Ignore the missing values.
# Note the use of the '.' function to allow variables to be used without quoting.
daystats <- ddply(activity, .(date), summarize, 
                  daysum = sum(steps, na.rm = TRUE),
                  daymean = round(mean(steps, na.rm = TRUE),1),
                  daymedian = round(median(steps, na.rm = TRUE),1))
# Show a sampling of the total number of steps taken per day, variable "daysum".
head(daystats)

# 2. Histogram of the total number of steps taken each day.
# Red line is the mean.
plot(daystats$date, daystats$daysum
     , type="h"
     , main="Steps Taken Per Day"
     , xlab="Date"
     , ylab="Steps"
     , col="blue"
     , lwd=2
)
abline(h=mean(daystats$daysum, na.rm=TRUE), col="red", lwd=2)

# 3. Report the mean and median total number of steps taken per day.
paste("Mean steps taken per day = ", round(mean(daystats$daysum, na.rm=TRUE),2))
paste("Median steps taken per day = ", median(daystats$daysum, na.rm=TRUE))

### What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis).
intstats <- ddply(activity, .(interval), summarize, 
                 intsum = sum(steps, na.rm = TRUE),
                 intmean = round(mean(steps, na.rm = TRUE),1))

plot.ts(intstats$interval, intstats$intmean 
        ,type="l" 
        ,xlab="5 minute interval" 
        ,ylab="average steps taken"
        ,main="Average Steps Taken per Interval")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
maxsteps <- which(intstats$intmean == max(intstats$intmean))
paste("Which 5-minute interval contains the maximum number of steps?")
intstats[maxsteps,]

### Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset.
any(is.na(activity$steps))
sum(is.na(activity$steps))


# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activitynona <- merge(activity, intstats, by = "interval")

# 2. Fill in missing values with daily average
activitynona$steps[is.na(activitynona$steps)] <-  
    activitynona$intmean[is.na(activitynona$steps)]

# Calculate statistics of steps taken per day, imputed values, for later use.
daystatsnona <- ddply(activitynona, .(date), summarize, 
                      daysum = sum(steps, na.rm = TRUE),
                      daymean = round(mean(steps, na.rm = TRUE),1),
                      daymedian = round(median(steps, na.rm = TRUE),1))

# 4a. Make a histogram of the total number of steps taken each day with NAs imputed.
# Red line is the mean.
plot(daystatsnona$date, daystatsnona$daysum
     , type="h"
     , main="Steps Taken Per Day with NAs Imputed"
     , xlab="Date"
     , ylab="Steps per Day"
     , col="blue"
     , lwd=2
)
abline(h=mean(daystatsnona$daysum, na.rm=TRUE), col="red", lwd=2)

# 4b. Report the mean and median total number of steps taken per day.
paste("Mean steps taken per day = ", round(mean(daystatsnona$daysum),2))
paste("Median steps taken per day = ", median(daystatsnona$daysum))
# 4c. Do these values differ from the estimates from the first part of the assignment?
identical(daystats, daystatsnona)
paste("The imputed data set is not identical (FALSE) to the original.")
# 4d. What is the impact of imputing missing data on the estimates of the total daily number of steps?
paste("Imputing values increased(decreased) the average daily steps by ",
      round(mean(daystats$daysum),2) - round(mean(daystatsnona$daysum),2))

### Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels - 
# "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity3 <- mutate(activitynona, dayofweek = weekdays(activitynona$date))
activity4 <- mutate(activity3, daycat = ifelse(grepl('^S',activity3$dayofweek)==TRUE,"weekend","weekday"))

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
#(x-axis) and the average number of steps taken, averaged across all weekday days 
# or weekend days (y-axis).
# Calculate statistics for day category (weekday or weekend). The sum will be used in the graphs.
daycatstats <- ddply(activity4, .(interval,daycat), summarize, 
                  daycatsum = sum(steps, na.rm = TRUE),
                  daycatmean = round(mean(steps, na.rm = TRUE),1))

library(lattice)
xyplot(daycatsum~interval|daycat, daycatstats 
       ,type="l"
       ,xlab="Interval" 
       ,ylab="Number of steps"
       ,layout=(c(1,2))
       )
