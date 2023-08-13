# Cyclistic_Case_Study
author: "Ellen Bridgeman"
date: "2023-08-11"

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction

Hello, and welcome to my very first case study! Here, I will be performing real-world tasks of a junior data analyst while working for Cyclistic's marketing analyst team. The director of marketing, Lily Moreno, believes the company’s future success depends on maximizing the number of annual memberships. Therefore, my team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members. In order to answer the key business questions, I will follow the steps of the data analysis process: ask, prepare, process, analyze, share, and act. 

## About Cyclistic

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members.

## Ask

Moreno has set a clear business ask: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are
interested in analyzing the Cyclistic historical bike trip data to identify trends.

Moreno has assigned me the first question to answer: How do annual members and casual riders use Cyclistic bikes
differently?

## Prepare

I will use Cyclistic’s historical trip data to analyze and identify trends. Because this is a fictional scenario, the datasets will have a different name from Cyclistic and has been made available by Motivate International Inc. A limitation to this dataset is that data-privacy issues prohibit me from using riders’ personally identifiable information. This
means that I won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the
Cyclistic service area or if they have purchased multiple single passes.

Access the Cyclistic trip data [here](https://divvy-tripdata.s3.amazonaws.com/index.html) 


```{r}
#Loading packages
library(tidyverse)  
library(lubridate)  
library(ggplot2)  
library(dplyr) 
library(tidyr)
```
```{r}
#Collect the data
q2_2019 <- read.csv("~/DAC8 Case Study 1/Extracted/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("~/DAC8 Case Study 1/Extracted/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("~/DAC8 Case Study 1/Extracted/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("~/DAC8 Case Study 1/Extracted/Divvy_Trips_2020_Q1.csv")

```
## Process

Now that I have my data stored and imported, I need to process the data by checking for errors, transform the data so I can work with it effectively, and document my full cleaning process.
```{r}
#Comparing column names for each file to see if they match.
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)
```
```{r results='hide'}
#The columns did not match, so I renamed the columns to make them consistent with q1_2020
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
```
``` {r results = 'hide'}
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

```
``` {r results = 'hide'}
(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"
                   ,rideable_type = "X01...Rental.Details.Bike.ID" 
                   ,started_at = "X01...Rental.Details.Local.Start.Time"  
                   ,ended_at = "X01...Rental.Details.Local.End.Time"  
                   ,start_station_name = "X03...Rental.Start.Station.Name" 
                   ,start_station_id = "X03...Rental.Start.Station.ID"
                   ,end_station_name = "X02...Rental.End.Station.Name" 
                   ,end_station_id = "X02...Rental.End.Station.ID"
                   ,member_casual = "User.Type"))
```
```{r results = 'hide'}

# Inspecting the dataframes to look for incongruities
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)
```
```{r}
# Converting ride_id and rideable_type to character type so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
```
```{r}
# Stacking individual datasets into one big data frame for easier analyses
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
```
```{r}
# Inspecting the new dataframe
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics.
```
Upon inspection, I noticed a few problems to fix:

1. In the "member_casual" column, there are two names for members, "member" and "Subscriber", and two names for casual riders, "Customer" and "casual".I will consolidate that from four to two labels for consistency.

2. The data can only be aggregated at the ride-level. I will add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.

3. I will add a calculated field for length of ride since the q1_2020 data did not have the "tripduration" column. I will add "ride_length" to the entire dataframe for consistency.

4. There are some rides where tripduration shows up as negative, including several hundred rides where Cyclistic took bikes out of circulation for quality Control. I will delete these rides.
```{r}
# Seeing how many observations fall under each usertype
table(all_trips$member_casual)
```
```{r}
# Reassigning to the desired values
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))
```
```{r}
# Checking to make sure the proper number of observations were reassigned
table(all_trips$member_casual)
```
```{r}
# Adding columns that list the date, month, day, and year of each ride
# This will allow me to aggregate ride data for each month, day, or year
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
```
```{r}
# Adding a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```
```{r}
# Inspect the structure of the columns
str(all_trips)
```
```{r}
# Convert "ride_length" from character to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length) #Check
```
```{r}
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Cyclistic or ride_length was negative
#creating a new data frame all_trips_v2 that excludes rows where either the start_station_name column is "HQ QR" or the ride_length column is less than 0.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```
## Analyze

My data is stored appropriately and has been prepared for analyses. Now it's time to aggregate the data so it is useful and accessible, organize and format the data, perform calculations, and identify trends and relationships.
```{r}
# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)
```
```{r}
# Comparing members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```
```{r}
# Checking the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
```{r}
# I noticed that the days of the week are out of order and fixed that
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
```{r}
# I ran the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
```{r}
# analyzing ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #adds a new column weekday to the data frame 
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
    ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts
```
### Summary of Analyses

To restate, I was asked, "how do annual members and casual riders use Cyclistic bikes
differently?". 

My insights:
1. Members log more rides than casual riders.
2. Members ride the bikes more often during the week and less on weekends.
3. Casual riders ride the bikes more often during the weekend and less during the week.
4. Members tend to take short and consistent rides.
5. Casual riders take longer rides with more variation.

Based on these insights, I would inform Moreno of my analysis:
Casual riders are using their rides for longer periods of time and mostly on weekends.This suggests to me that they are using their bikes more leisure or exercise. When casual riders rent the bike, they are renting it for a planned activity.
However member riders take shorter rides more frequently, and they ride more through the weekdays and less on weekends. This would indicate to me that members use the bikes for every day transportation, ie., riding to work or the store. 

## Share

Now that I have performed my analysis and gained some insights, I will share my findings by creating effective data visualizations and ensuring my work is accessible.
```{r echo=FALSE}
# Visualizing the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
    ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```
```{r echo=FALSE}
# Visualizing the average duration by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

## Act

After thorough analysis and interpretation of the data, My team has identified key insights and actionable recommendations that can contribute to casual riders converting into members. These recommendations are designed to leverage data-driven insights to drive informed decision-making and increase annual membership purchases.

Here are the main recommendations from our analysis:

**Targeted Marketing Strategies:**

Develop targeted marketing campaigns for the casual rider user group. Casual riders use the services for primarily leisure activities, weekend getaways, and special events. To have more casual riders convert to members, promote the convenience and practicality of bike usage for daily commutes and errands. 

**Membership Benefits Enhancement:**

Enhance the benefits offered to members to service a variety of user groups. Consider loyalty programs, rewards for frequent riders and rewards for long duration of rides. The goal is to benefit both leisure riders and daily commute riders so that the membership will appeal to the riding habits of casual riders.

**Weekday Incentives:**

Members are already riding during the week. To get more casual riders interested in daily commuting and encourage more weekday usage, introduce incentives such as discounted morning or evening rides for commuting purposes.
Each recommendation is supported by data-backed evidence and insights, providing a clear understanding of the underlying trends and factors that have informed our conclusions.

We understand that successful implementation requires collaboration and coordination across teams and departments. We are available to further discuss these recommendations, provide additional context, and answer any questions you may have. 

We are confident that these recommendations, if implemented effectively, have the potential to concert more casual riders to members. We look forward to the opportunity to contribute to the successful execution of these recommendations and to support the positive changes they can bring to Cyclistic.







