##Above all we set language preferences to English  and save current settings
## In order to save my russian environment
curr_locale <- Sys.getlocale("LC_TIME")

## To set it in english
Sys.setlocale("LC_TIME","en_US.UTF-8")

## To come back to local setting later
## Sys.setlocale("LC_TIME",curr_locale)

##Function to create x-axis
intName <- function(interval) {
        if (as.numeric(interval) < 10) {
                t_interval = paste("00:0",interval, sep="")
        } else if (as.numeric(interval) >= 10 && as.numeric(interval) < 100) {
                t_interval = paste("00:", interval, sep="")
        } else if (as.numeric(interval) >= 100 && as.numeric(interval) < 1000) {
                t_interval = paste("0", substr(interval,1,1),":", substr(interval,2,3), sep="")
        } else {
                t_interval = paste(substr(interval,1,2),":", substr(interval,3,4), sep="")
        }
}

##Setting working directory
setwd("./RR/Week1_CP/RepData_PeerAssessment1")

##Unzipping file fromm the forked repository
if(!file.exists('activity.csv')){
        unzip('activity.zip')
}
activity_data <- read.csv("activity.csv")

##Formatting dates as Date
activity_data$date <- as.Date(activity_data$date,"%Y-%m-%d")

##Q: What is mean total number of steps taken per day?
Total_steps_day <- tapply(activity_data$steps, activity_data$date,sum)

##Plotting the histogram (number of breaks was find by plotting several plots)
hist(Total_steps_day, breaks=10, col="violetred2",xlab="Number of steps", 
     ylab="Frequency", main="Histogram of Number of Total Steps taken per day")

Mean_tsd <- mean(Total_steps_day)
Median_tsd <- median(Total_steps_day)
##NA, cause we didnt omit that. Will do in next steps.
Mean_tsd2 <- mean(Total_steps_day, na.rm = TRUE)
Median_tsd2 <- median(Total_steps_day,na.rm = TRUE)
print(Mean_tsd2)
print(Median_tsd2)

##Q: What is the average daily activity pattern?

mean_steps <- tapply(activity_data$steps,activity_data$interval,
                    mean,na.rm=TRUE)
plot(names(mean_steps), mean_steps , type="l", xlab="5 minutes intervals", 
     ylab="Mean number of steps (during all Days)", 
     main="Average Steps Taken at 5 minute Intervals",
     col="blue")

## PLotting a bit more beautiful plot
yax<-names(mean_steps)
for (i in 1:length(names(mean_steps))){
                yax[i]<-intName(names(mean_steps)[i])
}

yax<-as.factor(yax)
plot(yax, mean_steps, xlab="5 minutes intervals", 
     ylab="Mean number of steps (during all Days)", 
     main="Average Steps Taken at 5 minute Intervals",
     col="blue")

##
##5-minutes interval with maximum steps taken
max_steps_interval <- which.max(mean_steps)
msi <- names(max_steps_interval)
print(msi)

##Q: Imputing missing values
##missing values
mis_val <- sum(is.na(activity_data$steps))
print(mis_val)

## As the strategy for filling NA's we took this: fill missing values with mean
## of 5-minutes intervals through all days
Activity_data_filled <- activity_data

for (i in 1:nrow(Activity_data_filled)){
        if (is.na(Activity_data_filled$steps[i]) == TRUE){
                Activity_data_filled$steps[i] <- mean_steps[which(
                        as.numeric(names(mean_steps)) == Activity_data_filled$interval[i])]
        }
}

Total_steps_day_filled <- tapply(Activity_data_filled$steps, Activity_data_filled$date,sum)

##Plotting the histogram (number of breaks was find by plotting several plots)
hist(Total_steps_day_filled, breaks=10, col="peachpuff",xlab="Number of steps", 
     ylab="Frequency", main="Histogram of Number of Total Steps taken per day, NA filled")

Mean_tsd_filled<- mean(Total_steps_day_filled)
Median_tsd_filled <- median(Total_steps_day_filled)
print(Mean_tsd_filled)
print(Median_tsd_filled)
##Mean is the same (surprise!), cause we used meanvalue of the intervals to substitute NA's
## and Median became a bit higher and equal to Mean

##Q: Are there differences in activity patterns between weekdays and weekends?

##Adding column with type of the day
Activity_data_filled$day_type <- weekdays(as.Date(Activity_data_filled$date))
Activity_data_filled$day_type <- ifelse(Activity_data_filled$day_type %in% c("Saturday", "Sunday"),"weekend", "weekday")
Activity_data_filled$day_type <-as.factor(Activity_data_filled$day_type)


##Aggegating by type of the day and time intervals
mean_tsd_final <- aggregate(Activity_data_filled$steps,
                       by=list(Activity_data_filled$interval,
                               Activity_data_filled$day_type),mean)

names(mean_tsd_final) <- c("interval","day_type","steps")

##Plotting results
library(lattice)
xyplot(steps ~ interval | day_type, data = mean_tsd_final, layout = c(1, 2), type="l", xlab = "Interval", ylab = "Number of steps")


library(ggplot2)
p <- ggplot(mean_tsd_final, aes(x = interval, y = steps))
p <- p + geom_line() + facet_grid(day_type ~ ., )
p <- p + ggtitle("Activity patterns on weekends and weekdays")
p + xlab("Interval") + ylab("Number of steps")
