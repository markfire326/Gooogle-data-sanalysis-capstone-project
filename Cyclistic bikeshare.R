---
title: "Cyclistic Bike Share Analysis"
author: "George Ogala"
date: "2/4/2022"
output: html_document
---

# STEP 1

## Installing packages that will aid our job delivery

install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("janitor")
install.packages("skimr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("readr")


## Loading installed packages

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(here)
library(readr)
library(tidyr)
library(ggplot2)

# STEP 3

## Uploading 12 csv files for cleaning and analysis
apr <- read_csv("C:/Users/gee/Desktop/bikeshare/202004-divvy-tripdata.csv")
may <- read_csv("C:/Users/gee/Desktop/bikeshare/202005-divvy-tripdata.csv")
jun <- read_csv("C:/Users/gee/Desktop/bikeshare/202006-divvy-tripdata.csv")
jul <- read_csv("C:/Users/gee/Desktop/bikeshare/202007-divvy-tripdata.csv")
aug <- read_csv("C:/Users/gee/Desktop/bikeshare/202008-divvy-tripdata.csv")
sep <- read_csv("C:/Users/gee/Desktop/bikeshare/202009-divvy-tripdata.csv")
oct <- read_csv("C:/Users/gee/Desktop/bikeshare/202010-divvy-tripdata.csv")
nov <- read_csv("C:/Users/gee/Desktop/bikeshare/202011-divvy-tripdata.csv")
dec <- read_csv("C:/Users/gee/Desktop/bikeshare/202012-divvy-tripdata.csv")
jan <- read_csv("C:/Users/gee/Desktop/bikeshare/202101-divvy-tripdata.csv")
feb <- read_csv("C:/Users/gee/Desktop/bikeshare/202102-divvy-tripdata.csv")
mar <- read_csv("C:/Users/gee/Desktop/bikeshare/202103-divvy-tripdata.csv")


# STEP 3

## Converting and unifying datatype
apr <-  mutate(apr, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

may <-  mutate(may, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

jun <-  mutate(jun, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

jul <-  mutate(jul, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

aug <-  mutate(aug, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

sep <-  mutate(sep, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

oct <-  mutate(oct, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

nov <-  mutate(nov, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

dec <-  mutate(dec, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

jan <-  mutate(jan, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

feb <-  mutate(feb, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

mar <-  mutate(mar, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))


# Merge the 12 different files into one data.frame

ridedata <- bind_rows(apr,may,jun,jul,aug,sep,oct,nov,dec,jan,feb,mar)

## Inspect the combined data

colnames(ridedata)
str(ridedata)
head(ridedata)

# STEP 4

## Clean dataset by removing unnessesary colons
ridedata <- ridedata %>% select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

## Add date, month, day, and trip length colons
ridedata$date <- as.Date(ridedata$started_at)
ridedata$month <- format(as.Date(ridedata$date), "%B")
ridedata$day <- format(as.Date(ridedata$date), "%A")
ridedata$trip_length <- difftime(ridedata$ended_at, ridedata$started_at)

# More data cleaning of new colons

## remove empty rows, dataset trimmed down from3489748 rows to 3294691
colSums(is.na(ridedata)) #returns a summary of empty colons on each variable
cleanridedata <- ridedata [complete.cases(ridedata), ]

## Remove all negative and zero trip length as well as NA/null values
cleanridedata <- subset(cleanridedata, trip_length>0)

## Convert ride length to integer
cleanridedata$trip_length <- as.integer(cleanridedata$trip_length)


# STEP 5

# DESCRIPTIVE ANALYSIS

## Aggregate data to determine average trip length of riders for each month of the year
aggregate(trip_length ~ member_casual + month, cleanridedata, mean)

## Aggregate data to determine average trip length of riders on each day of the week
aggregate(trip_length ~ day + member_casual, cleanridedata, mean)

## Aggregate data to determine how riders use the different bike types available
aggregate(trip_length ~ rideable_type + member_casual, cleanridedata, mean)


# STEP 6

## Group and visualize rider type by day of the week
mc_week <- cleanridedata %>% group_by(day, member_casual) %>%
  count()
ggplot(data=mc_week)+
  geom_col(mapping=aes(x=day, y=n, fill=member_casual))+
  facet_grid(~member_casual)+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Ride length by day of the week")+
  theme(axis.text.x=element_text(angle=45))

# Vizualization

## Group, summarize and visualize data to determine ride count by rider type
mcdata <- cleanridedata %>% group_by(member_casual) %>% count()
ggplot(data=mcdata)+
  geom_col(mapping=aes(x=member_casual, y=n, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Number of trips by different rider types")

## Summarize and visualize data by rider type and average trip length to ascertain lenght of ride taken by different rider types
trip_len <- cleanridedata %>% group_by(member_casual) %>% summarise(mean_trip_length = mean(trip_length, na.rm = TRUE))
ggplot(data=trip_len)+
  geom_col(mapping=aes(x=member_casual, y=mean_trip_length, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Lenght of ride taken by different rider types")

## Group and visualize data by member_casual and rideable_type to determine prefered type of bike by riders
ride_type <- cleanridedata %>% group_by(member_casual, rideable_type) %>%
  count()
ggplot(data=ride_type)+
  geom_col(mapping=aes(x=rideable_type, y=n, fill=member_casual))+
  facet_wrap(~member_casual)+
  labs(y="Number of rides", x="Type of bike", fill="Member/Casual",
       title="Most prefered type of bike by riders")


                            # TOP 10 STATIONS

## Creat a new data frame for all stations
all_stations <- bind_rows(data.frame("stations" = cleanridedata$start_station_name, 
                                     "member_casual" = cleanridedata$member_casual),
                          data.frame("stations" = cleanridedata$end_station_name,
                                     "member_casual" = cleanridedata$member_casual))

# Exclude entries with no station name
all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]

# Separate the data frame by rider type
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]


# Get the top 10 popular stations all, members and casual riders
top_10_station <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)

## Viz
ggplot(data=top_10_station)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations.")+
  theme(axis.text = element_text(angle=45))

# Get the top 10 popular stations for members
top_10_stations_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

## Viz
ggplot(data=top_10_stations_member)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
  theme(axis.text = element_text(angle=45))

# Get the top 10 popular stations for members
top_10_stations_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

## Viz
ggplot(data=top_10_stations_casual)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
  theme(axis.text = element_text(angle=45))



# END
