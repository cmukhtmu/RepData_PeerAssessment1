library(data.table)
library(ggplot2)
#library(gridExtra)
#library(ggpubr)
#library(dplyr)

req4 <- function()
{
  # extracting the zip file    
  if(!file.exists("./activity.csv"))
  {
    print("Extracting zip file, please wait...")
    unzip("./activity.zip")
    print("Zip file extracted successfully!")
  }
  
  if(file.exists("./activity.csv"))
  {
    # reading the csv file in a data frame
    print("Reading data from CSV file, please wait...")
    activity = read.csv("activity.csv")
    print("CSV file read successfully!")
    # print(head(activity))
    
    print(sum(is.na(activity)))
    steps_by_interval <- aggregate(steps ~ interval, activity, mean)
    by_day <- group_by(activity, date)
    steps_by_day <- summarise(by_day, total = sum(steps))
    
    data_imputed <- activity
    for (i in 1:nrow(data_imputed)) {
      if (is.na(data_imputed$steps[i])) {
        interval_value <- data_imputed$interval[i]
        steps_value <- steps_by_interval[
          steps_by_interval$interval == interval_value,]
        data_imputed$steps[i] <- steps_value$steps
      }
    }
    
    df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
    print(head(df_imputed_steps_by_day))
    
    data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
    data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
    data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"    
    
    data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)
    
    # calculate average steps by interval across all days
    df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)
    
    # creat a plot
    qplot(interval, 
          steps, 
          data = df_imputed_steps_by_interval, 
          type = 'l', 
          geom=c("line"),
          xlab = "Interval", 
          ylab = "Number of steps", 
          main = "") +
      facet_wrap(~ type_of_day, ncol = 1)
    
  }
}