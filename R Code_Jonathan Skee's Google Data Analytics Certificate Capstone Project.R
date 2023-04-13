#Welcome to Jonathan Skee's Google Data Analytics Certificate Capstone Project R code! 
#Here are the steps I took in R to help address the business task...

#First, I installed and opened the necessary packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("getwd")
install.packages("dplyr")
install.packages("scales")

library(tidyverse) #Helps handle data
library(lubridate) #Helps with date attributes
library(ggplot2) #Visualizes the data
getwd() #Displays working directory
library(dplyr) #Helps manipulate data set
library(scales) #Helps with data visualization


#Next, I uploaded the data
#I already merged the data files beforehand using Microsoft Command Prompt
#I already performed data cleaning already in Excel, such as removing duplicates and rows with missing data
#I also double checked this cleaning using SQL

cyclistic_trip_data <- read_csv("C:\\Users\\jskee\\OneDrive - Demeter Analytics Services, LLC\\Documents\\Google Data Analytics Certificate Capstone Project\\Cyclistic_Trip_Data_02-2022_01-2023\\cyclistic-merged-csv-files.csv")

#Next, I inspected the new table that was created to ensure everything transferred correctly

colnames(cyclistic_trip_data) #Inspected columns. Looks good
nrow(cyclistic_trip_data) # Confirmed number of rows. All 837,386 transferred correctly
dim(cyclistic_trip_data) #Confirmed size of the data frame: 837,386 rows by 15 columns
head(cyclistic_trip_data) #Looked at first 6 rows to confirm data looks good
str(cyclistic_trip_data) #Confirmed the data types of all columns
summary(cyclistic_trip_data) #Provided a summary of statistics regarding the data

#Next, I needed find a better way to aggregate the data, as the ride-level was too granular
#I added additional columns for day, month, and year to better aggregate the data
cyclistic_trip_data$date <- format(as.POSIXct(cyclistic_trip_data$started_at,format='%m/%d/%Y'),format='%m/%d/%Y') #Made the date formatting consistent

cyclistic_trip_data$month <- month(mdy(cyclistic_trip_data$date)) #Added a month column
cyclistic_trip_data$day <- day(mdy(cyclistic_trip_data$date)) #Added a day column
cyclistic_trip_data$year <- year(mdy(cyclistic_trip_data$date)) #Added a year column

str(cyclistic_trip_data) #Inspected the structure of the data. Looks good

#Next, in order to analyze ride_length, I needed to take the hms column and add a minutes and seconds column (numeric data type)

cyclistic_trip_data$ride_length_char <- as.character(cyclistic_trip_data$ride_length) #Needed to convert to char first
cyclistic_trip_data$hms <- hms(cyclistic_trip_data$ride_length_char) #Needed to convert to hms
cyclistic_trip_data$seconds <- period_to_seconds(cyclistic_trip_data$hms) #Converted hms to seconds
cyclistic_trip_data <- cyclistic_trip_data %>% mutate(ride_time_in_mins = seconds / 60) #converted seconds to minutes for smoother calculations

cyclistic_trip_data <- cyclistic_trip_data %>%
  filter(!is.na(seconds)) #Filtered out 3 null cells in the data set that were causing errors


#Now, it's time to analyze the data with descriptive statistics

mean(cyclistic_trip_data$ride_time_in_mins) #Calculated average (total ride length / rides)
median(cyclistic_trip_data$ride_time_in_mins) #Calculated midpoint ride time in the ascending array of rides
max(cyclistic_trip_data$ride_time_in_mins) #Calculated longest ride
min(cyclistic_trip_data$ride_time_in_mins) #Calculated shortest ride

summary(cyclistic_trip_data$ride_time_in_mins) #Summarized descriptive analysis

#Next, I needed to compare Cyclistic's members and casual users to identify differences in trends
#This set of code aggregated descriptive analysis for members and casual users
aggregate(cyclistic_trip_data$ride_time_in_mins ~ cyclistic_trip_data$member_casual, FUN = mean)
aggregate(cyclistic_trip_data$ride_time_in_mins ~ cyclistic_trip_data$member_casual, FUN = median)
aggregate(cyclistic_trip_data$ride_time_in_mins ~ cyclistic_trip_data$member_casual, FUN = max)
aggregate(cyclistic_trip_data$ride_time_in_mins ~ cyclistic_trip_data$member_casual, FUN = min)

#I then needed to create a new column for day_of_week with day names instead of numbers for visualization purposes
cyclistic_trip_data <- cyclistic_trip_data %>% mutate(day_of_week_char = case_when(
  day_of_week == 1 ~ 'Sunday',
  day_of_week == 2 ~ 'Monday',
  day_of_week == 3 ~ 'Tuesday',
  day_of_week == 4 ~ 'Wednesday',
  day_of_week == 5 ~ 'Thursday',
  day_of_week == 6 ~ 'Friday',
  day_of_week == 7 ~ 'Saturday'
))

#Reordered the days of the week in the variable so they'd display properly
cyclistic_trip_data$day_of_week_char <- factor(cyclistic_trip_data$day_of_week_char, levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

#Calculated the average ride time for each day of the week for both members and casual users
aggregate(cyclistic_trip_data$ride_time_in_mins ~ cyclistic_trip_data$member_casual + cyclistic_trip_data$day_of_week, FUN = mean)

#Analyzed rider data by type and day of the week
summary_data <- cyclistic_trip_data %>% 
  group_by(member_casual, day_of_week_char) %>% #Grouped by rider type and weekday
  summarize(number_of_rides = n() #Calculated the number of rides
            ,average_duration = mean(ride_time_in_mins)) %>% #Calculated the average trip duration
  arrange(member_casual, day_of_week_char)	#Sorted the data	


#Now, it's time to visualize the data!

#Visualized the number of rides by rider type and day of the week in ggplot2
summary_data %>%  
  ggplot(aes(x = day_of_week_char, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +  #Places the two bars side by side
  ggtitle("Comparing Rider Types by Total Number of Rides for Each Day of Week") + #Adds title
  theme(plot.title = element_text(hjust = 0.5)) +  #Centers the title
  xlab("") +  #Deletes the default x-axis label
  ylab("Number of Rides") +  #Replaces default y-axis label
  scale_y_continuous(labels = scales::comma) +  #Adds comma separator for every thousand on the y-axis
  labs(fill="Rider Type") +  #Replaces default legend label
  geom_text(aes(label = comma(number_of_rides)), position = position_dodge(width = 0.9), vjust = 1.5, color = 'black', size = 3) +  #Adds bar labels
  scale_fill_discrete(labels=c('Casual', 'Member'))  #Replaces default legend labels

#Visualized the number of rides by average duration and day of the week in ggplot2
summary_data %>%  
  ggplot(aes(x = day_of_week_char, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Comparing Rider Types by Average Ride Duration for Each Day of Week") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Average Ride Duration (Minutes)") +
  scale_y_continuous(labels = scales::comma) +
  labs(fill="Rider Type") +
  geom_text(aes(label = comma(average_duration)), position = position_dodge(width = 0.9), vjust = 1.5, color = 'black', size = 3) +
  scale_fill_discrete(labels=c('Casual', 'Member'))

#Lastly, I needed to export a summary file for further analysis in Tableau
write.csv(summary_data, file = 'C:\\Users\\jskee\\OneDrive - Demeter Analytics Services, LLC\\Documents\\Google Data Analytics Certificate Capstone Project\\avg_ride_length.csv')












