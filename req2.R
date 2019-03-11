library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)

req2 <- function()
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
    
    steps_by_interval <- aggregate(steps ~ interval, activity, mean)
    
    # create a time series plot 
    plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
         main="Average number of steps over all days", xlab="Interval", 
         ylab="Average number of steps")
    max_steps_row <- which.max(steps_by_interval$steps)
    
    # find interval with this max
    steps_by_interval[max_steps_row, ]
    
    print(paste("interval:", steps_by_interval[max_steps_row, 1]))
  }
}