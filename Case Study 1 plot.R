
trip_data <- read.csv('avg_ride_length.csv')

trip_data

library(tidyverse)
library(ggplot2)
library(lubridate)


trip_data$ride_length <- trip_data$ride_length/60
trip_data$day_of_week <- factor(trip_data$day_of_week, levels = c("Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday")

trip_data

data <- trip_data %>% 
  ggplot(aes(x = day_of_week, y = ride_length, fill = rider_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("salmon1","seagreen")) + 
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(y = "Average Duration (mins)", x = "Day of Week", title = "Average trip duration of riders between April 2019 to March 2020") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 10, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 15, b = 10, l = 0)),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

data + labs(fill = "Rider type") #change legend title

#test push

         
