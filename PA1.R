
# REPRODUCIBLE RESEARCH - PROGRAMMING ASSIGMENT NO. 1

# 1 Code for reading in the dataset and/or processing the data

setwd("C:/Users/Javier/Downloads")
file1 <- read.csv("read.csv") # original file
names(file1) # titles of readed file
file2 <- file1[complete.cases(file1),] # file without NA values

# 2 Histogram of the total number of steps taken each day

d1 <- duplicated(file2$date)
d2 <- file2$date[!d1]
days <- as.vector(d2) # 1 to 53
 
steps_sum <- as.vector(1:53)

for(i in 1:53)
{
  steps_sum[i] <- sum(file2$steps[file2$date == days[i]])  
}

hist(steps_sum,
     main = 'Total steps by day',
     xlab = 'Steps',
     col="gray"
     )

# 3 Mean and median number of steps taken each day

round(mean(steps_sum),0)
round(median(steps_sum),0)

# 4 Time series plot of the average number of steps taken

steps_mean <- as.vector(1:53)

for(ii in 1:53)
{
  steps_mean[ii] <- round(mean(file2$steps[file2$date == days[ii]]),2)
}

hist(steps_mean,
     main = 'Mean steps by day',
     xlab = 'Steps',
     col="red"
)

# 5 The 5-minute interval that, on average, contains the maximum number of steps

interval1 <- duplicated(file2$interval)
interval <- file2$interval[!interval1] # 1 to 288

interval_mean <- 1:288

for(iii in 1:288)
{
  interval_mean[iii] = round(mean(file2$steps[file2$interval == interval[iii]]),2)
}   

interval_steps <- as.data.frame(cbind(interval,interval_mean))

interval_steps2 <- interval_steps[order(-interval_steps$interval_mean),]
head(interval_steps2)

# 6 Code to describe and show a strategy for imputing missing data

file1 <- read.csv("read.csv") # original file
names(file1) # titles of readed file
nrow(file1)

file2 <- file1[complete.cases(file1),] # file without NA values
names(file2)
nrow(file2)

table(is.na(file1$steps))
table(is.na(file1$date))
table(is.na(file1$interval))

steps_na <- 1:17568

for(s in 1:17568) # fill of the NA values with mean by interval
{
  if (is.na(file1$steps[s])) 
    { 
      steps_na[s] = interval_steps2$interval_mean[interval_steps2$interval == file1$interval[s]]
    } 
    else
    {
      steps_na[s] = file1$steps[s]
    }  
}

table(is.na(steps_na))


# create the new dataset with the filled NA values

file3 <- cbind(file1,steps_na)

paste("Original dataset rows: ",nrow(file1))

paste("New dataset rows: ", nrow(file3))




# 7 Histogram of the total number of steps taken each day after missing values are imputed

steps_sum <- as.vector(1:53)

for(vi in 1:53)
{
  steps_sum[vi] <- sum(file3$steps_na[file3$date == days[vi]])  
}

steps_sum

hist(steps_sum,
     main = 'Total steps by day',
     xlab = 'Steps',
     col="orange"
)



# 8 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

week_day <- 1:17568

for(iiii in 1:17568)
{
  if(weekdays(as.Date(file3$date[iiii])) == "lunes") { week_day[iiii] = 1 }
  if(weekdays(as.Date(file3$date[iiii])) == "martes") { week_day[iiii] = 1 }
  if(weekdays(as.Date(file3$date[iiii])) == "miércoles") { week_day[iiii] = 1 }
  if(weekdays(as.Date(file3$date[iiii])) == "jueves") { week_day[iiii] = 1 }
  if(weekdays(as.Date(file3$date[iiii])) == "viernes") { week_day[iiii] = 1 }
  if(weekdays(as.Date(file3$date[iiii])) == "sábado") { week_day[iiii] = 2 }
  if(weekdays(as.Date(file3$date[iiii])) == "domingo") { week_day[iiii] = 2 }
}

table(week_day)

file4 <- cbind(file3,week_day)

head(file4)


# plot of the average of steps by time interval

interval_week_day <- 1:288
interval_week_end <- 1:288

for (i_day in 1:288)
{
  interval_week_day[i_day] = round(mean(file4$steps_na[file4$interval == interval[i_day] & file4$week_day[i_day] == 1]),2)
  
  }

hist(interval_week_day,
     main = 'Average steps by interval - weekday',
     xlab = 'Steps',
     col="purple"
)


# average per weekday and weekend

paste("weekday steps average: ",round(mean(file4$steps_na[file4$week_day == 1]),2))

paste("weekend steps average: ",round(mean(file4$steps_na[file4$week_day == 2]),2))
