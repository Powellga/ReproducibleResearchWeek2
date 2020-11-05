library(data.table)
library(dplyr)
library(ggplot2)

setwd("C:/_TEMP/_R_WORK_TEMP/Johns_Hopkins_Data_Science/Module 5-Reproducable_Research/Week2/peer_assignment/")
data <- fread("C:/_TEMP/_R_WORK_TEMP/Johns_Hopkins_Data_Science/Module 5-Reproducable_Research/Week2/peer_assignment/activity.csv")


#remove all rows with NA or with "" (nothing)
data_no_NA <- data[!(is.na(data$steps) | data$steps==""), ]
#calculate the number of steps and print
mean_steps <- mean(data_no_NA$steps)
mean_steps

#######################################################################################
histo_steps <- ggplot(data=data_no_NA) +
        geom_bar(aes(x=factor(date), y=steps), stat= "identity", fill = "blue", width = 0.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "Date") + labs(y = "Total Steps") + labs(title = "Total Steps Taken Each Day") 
histo_steps

#############################################################################
data_no_NA$steps <- as.numeric(data_no_NA$steps)
mean_steps_table <- data_no_NA %>% group_by(date) %>% summarise(mean(steps), median(steps))
colnames(mean_steps_table)[2] <- "MeanSteps"
colnames(mean_steps_table)[3] <- "MedianSteps"
mean_steps_table 
#strangly, the median number of steps is in fact 0 - 
#doublecheck median steps:
median(data_no_NA$steps)

############################################################################

avg_daily_activity_pattern <- data_no_NA %>% group_by(interval) %>% summarise(mean(steps))
colnames(avg_daily_activity_pattern)[2] <- "MeanSteps"

time_plot <- ggplot(data=avg_daily_activity_pattern) +
        geom_line(aes(x=interval, y=MeanSteps), color = "#00AFBB", size = 1) +
        labs(x = "5-min Interval") + labs(y = "Average Steps") + labs(title = "Average Daily Steps Taken during each 5-min Interval ")
time_plot

#############################################################################


sorted_avg_daily_activity_pattern <- avg_daily_activity_pattern[order(-avg_daily_activity_pattern$MeanSteps),]
sorted_avg_daily_activity_pattern
sorted_avg_daily_activity_pattern[1,1]

##############################################################################

sum(is.na(data$interval))
sum(is.na(data$date))
sum(is.na(data$steps))    



##########################################################################

data_noZ <- data

#before:
head(data_noZ)

#replace all NAs with 0
data_noZ[is.na(data_noZ)] <- 0

#after:
head(data_noZ)

#######################################################################

histo_steps2 <- ggplot(data=data_noZ) +
        geom_bar(aes(x=factor(date), y=steps), stat= "identity", fill = "blue", width = 0.5) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "Date") + labs(y = "Total Steps") + labs(title = "Total Steps Taken Each Day") 
histo_steps2

#######################################################################

#sets the data_no_NA$steps variable as numeric
data_noZ$steps <- as.numeric(data_noZ$steps)
#creates a new table  from the data_no_NA data.table that finds the median and average steps grouped by date using dplyr
mean_steps_table_noZ <- data_noZ %>% group_by(date) %>% summarise(mean(steps))
colnames(mean_steps_table_noZ)[2] <- "MeanSteps"
mean_steps_table_noZ 

######################################################################

#create a new column in data_no_NA called "weekday" and use it to show which date was on what day
data_no_NA$weekday <- weekdays(data_no_NA$date)
#create another column in data_no_NA called "weekend". Test each value in weekday - it weekday is a saturday or sunday, then weekend is 1, else 0
data_no_NA$weekend <- ifelse(data_no_NA$weekday == "Saturday" | data_no_NA$weekday == "Sunday", "weekend", "weekday")

########################################################################


#creates two new data tables - one for weekdays and the other weekends
data_no_NA_weekend <- subset(data_no_NA, data_no_NA$weekday == "Saturday" | data_no_NA$weekday == "Sunday")
data_no_NA_weekday <- subset(data_no_NA, !data_no_NA$weekday == "Saturday" & !data_no_NA$weekday == "Sunday")

########################################################################

####Weekend
avg_daily_activity_pattern_weekend <- data_no_NA_weekend %>% group_by(interval) %>% summarise(mean(steps))
colnames(avg_daily_activity_pattern_weekend)[2] <- "MeanSteps"

time_plot_weekend <- ggplot(data=avg_daily_activity_pattern_weekend) +
        geom_line(aes(x=interval, y=MeanSteps), color = "#00AFBB", size = 1) +
        labs(x = "5-min Interval") + labs(y = "Average Steps") + labs(title = "Average Daily Steps Taken during each 5-min Interval (Weekend)")
  

####Weekday
avg_daily_activity_pattern_weekday <- data_no_NA_weekday %>% group_by(interval) %>% summarise(mean(steps))
colnames(avg_daily_activity_pattern_weekday)[2] <- "MeanSteps"

time_plot_weekday <- ggplot(data=avg_daily_activity_pattern_weekday) +
        geom_line(aes(x=interval, y=MeanSteps), color = "#00AFBB", size = 1) +
        labs(x = "5-min Interval") + labs(y = "Average Steps") + labs(title = "Average Daily Steps Taken during each 5-min Interval (Weekday)")


source("http://peterhaschke.com/Code/multiplot.R")
multiplot(time_plot_weekend, time_plot_weekday, cols=1)

