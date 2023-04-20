install.packages("tidyverse")
library(tidyverse)

## read csv files
apr <- read_csv("202204.csv")
may <- read_csv("202205.csv")
jun <- read_csv("202206.csv")
jul <- read_csv("202207.csv")
aug <- read_csv("202208.csv")
sep <- read_csv("202209.csv")
oct <- read_csv("202210.csv")
nov <- read_csv("202211.csv")
dec <- read_csv("202212.csv")
jan <- read_csv("202301.csv")
feb <- read_csv("202302.csv")
mar <- read_csv("202303.csv")


## merge the data
cyclistic <- rbind(apr, may, jun, jul, aug, sep, oct, nov, dec, jan, feb, mar)

## change the 'started_at' & 'ended_at' column which is in character to POSIXct
cyclistic$started_at <- as.POSIXct(cyclistic$started_at, format = "%m/%d/%Y %H:%M")
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at, format = "%m/%d/%Y %H:%M")

## calculate day of the week
cyclistic$day_of_the_week <- wday(cyclistic$started_at)

## calculate average ride length in minutes
cyclistic$ride_length_mins <- difftime(cyclistic$ended_at, cyclistic$started_at, units = "mins")

## Extract hour from started_at
cyclistic$hour <- format(as.POSIXct(cyclistic$started_at), format = "%H")

## Extract month from started_at
cyclistic$month <- format(as.POSIXct(cyclistic$started_at), format = "%m")

## calculate number of trips
num_of_trips <- cyclistic %>%
  group_by(day_of_the_week, member_casual) %>%
  summarize(total_rides = n())

## save the dataframe as csv file
write.csv(num_of_trips, file = "num_of_trips.csv", row.names=FALSE)

##plot
num_of_trips %>%
  group_by(member_casual, day_of_the_week) %>%
  arrange(member_casual, day_of_the_week)%>%
  ggplot(aes(x = day_of_the_week, y = total_rides, fill=member_casual)) + geom_col(position="dodge2") + 
  labs(title="Total number of Rides by Day", x="Day of the week", y = "Number of rides")


## calculate mean of the average length
avg_ride_length <- aggregate(cyclistic$ride_length_mins ~ cyclistic$day_of_the_week + cyclistic$member_casual, FUN=mean)

## save the dataframe as csv file
write.csv(avg_ride_length , file = "ride_length.csv", row.names=FALSE)

## plot
ride_length %>%
  group_by(member_casual, day_of_the_week) %>%
  arrange(member_casual, day_of_the_week)%>%
  ggplot(aes(x = day_of_the_week, y = ride_length_mins, fill=member_casual)) + geom_col(position="dodge2") + 
  labs(title="Average ride length in minutes", x="Day of the week", y = "Average ride length")


## calculate number of rides per hour
rides_by_hour <- cyclistic %>%
  group_by(hour, member_casual) %>%
  summarize(total_count = n())

## create column to convert 24hr time format to 12hr format
rides_by_hour$time <- case_when(rides_by_hour$hour == "00" ~ "12AM", 
                                rides_by_hour$hour == "01" ~ "1AM",
                                rides_by_hour$hour == "02" ~ "2AM",
                                rides_by_hour$hour == "03" ~ "3AM",
                                rides_by_hour$hour == "04" ~ "4AM",
                                rides_by_hour$hour == "05" ~ "5AM",
                                rides_by_hour$hour == "06" ~ "6AM",
                                rides_by_hour$hour == "07" ~ "7AM",
                                rides_by_hour$hour == "08" ~ "8AM",
                                rides_by_hour$hour == "09" ~ "9AM",
                                rides_by_hour$hour == "10" ~ "10AM",
                                rides_by_hour$hour == "11" ~ "11AM",
                                rides_by_hour$hour == "12" ~ "12PM",
                                rides_by_hour$hour == "13" ~ "1PM",
                                rides_by_hour$hour == "14" ~ "2PM",
                                rides_by_hour$hour == "15" ~ "3PM",
                                rides_by_hour$hour == "16" ~ "4PM",
                                rides_by_hour$hour == "17" ~ "5PM",
                                rides_by_hour$hour == "18" ~ "6PM",
                                rides_by_hour$hour == "19" ~ "7PM",
                                rides_by_hour$hour == "20" ~ "8PM",
                                rides_by_hour$hour == "21" ~ "9PM",
                                rides_by_hour$hour == "22" ~ "10PM",
                                rides_by_hour$hour == "23" ~ "11PM")

## save the dataframe as csv file
write.csv(rides_by_hour, file = "rides_by_hour.csv", row.names=FALSE)

## plot
ride_by_hour %>%
  group_by(member_casual, time) %>%
  ggplot(aes(x = hour, y = total_count, fill = member_casual)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Number of rides by hour",  x = "Hour", y = "Number of Rides")


## calculate rides by month
rides_by_month <- cyclistic %>%
  group_by(month, member_casual) %>%
  summarize(total_count = n())

## create column for month name
rides_by_month$month_name <- case_when(rides_by_month$month == "01" ~ "January",
                                       rides_by_month$month == "02" ~ "February",
                                       rides_by_month$month == "03" ~ "March",
                                       rides_by_month$month == "04" ~ "April",
                                       rides_by_month$month == "05" ~ "May",
                                       rides_by_month$month == "06" ~ "June",
                                       rides_by_month$month == "07" ~ "July",
                                       rides_by_month$month == "08" ~ "August",
                                       rides_by_month$month == "09" ~ "September",
                                       rides_by_month$month == "10" ~ "October",
                                       rides_by_month$month == "11" ~ "November",
                                       rides_by_month$month == "12" ~ "December")

## save the dataframe as csv file
write.csv(rides_by_month, file = "rides_by_month.csv", row.names=FALSE)

## plot
ride_by_month %>%
  group_by(member_casual, month) %>%
  ggplot(aes(x = month, y = total_count, fill = member_casual)) + 
  geom_col(position = "dodge2") + 
  labs(title = "Number of rides by month",  x = "month", y = "Number of Rides")

## calculate rides by member
rides_by_member <- cyclistic %>%
  group_by(member_casual) %>%
  summarize(total_count = n())

## save the dataframe as csv file
write.csv(rides_by_member, file = "rides_by_member.csv", row.names=FALSE)

