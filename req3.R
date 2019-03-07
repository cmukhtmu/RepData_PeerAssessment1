library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)

req3 <- function()
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
    
    hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
         xlab="Total number of steps in a day")
    
    print(mean(df_imputed_steps_by_day$steps))
    print(median(df_imputed_steps_by_day$steps))
    
    
  }
}