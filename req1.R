library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)

req1 <- function()
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
    activity = read.csv("activity.csv", header = T)
    print("CSV file read successfully!")
    # print(head(activity))
    
    activity$date <- as.Date(as.character(activity$date))
    by_day <- group_by(activity, date)
    steps_by_day <- summarise(by_day, total = sum(steps))
    
    activity_by_day = group_by(activity, date) %>%
        summarise(total = sum(steps))
    
    
    hist(steps_by_day$total, main="Histogram of total number of steps per day", 
         xlab="Total number of steps in a day")

    hist(activity_by_day$total, main="Histogram of total steps per day", 
         xlab="Total steps in a day")
    
    mysummary = summary(steps_by_day)
    
    print(mysummary[3,2])
    print(summary(activity_by_day))
    
  }
}