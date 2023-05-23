
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(dplyr)

getwd()

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

(q4_2019 <- rename(q4_2019,
                    ride_id = trip_id
                    ,rideable_type = bikeid
                    ,started_at = start_time
                    ,ended_at = end_time
                    , start_station_name = from_station_name
                    , start_station_id = from_station_id
                    , end_station_name = to_station_name
                    , end_station_id = to_station_id
                    , member_casual = usertype))


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

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  , rideable_type = as.character(rideable_type))

all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
all_trips

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

#Data cleaning
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
str(all_trips)
summary(all_trips)

table(all_trips$member_casual)
all_trips

all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                , "Subscriber" = "member"
                                , "Customer" = "casual")) #changes the names but doesnt create new columns


table(all_trips$member_casual)

head(all_trips)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m") #to get the month
all_trips$day <- format(as.Date(all_trips$date), "%d") #get the day
all_trips$year <- format(as.Date(all_trips$date), "%Y") # get year
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #get day of the week

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

str(all_trips)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) 
is.numeric(all_trips$ride_length) #check if ride_length is numeric

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v2
nrow(all_trips_v2)


nrow(all_trips_v2$start_station_id)

all_trips_v2$start_station_id

all_trips_v2 %>% sum(!is.na("start_station_id"))
all_trips_v2 %>% count(!is.na("end_station_id"))

count(all_trips_v2)



