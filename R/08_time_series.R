# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Time-series implementation
# First Version 2022-07-21
# --------------------------------------------------#

# Loading needed packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")
#Date format
#First, checking the class
class(covid$date)
# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)
# Now we can make numeric operations
range(covid$date)

covid$new_confirmed[covid$new_confirmed < 0] <- 0 #Because we have negative cases (just after checking and being sure you can do this)

#Rolling mean with zoo
covid$roll_mean<-zoo::rollmean(covid$new_confirmed, 14, fill=NA) #NA needed for removing empty spaces and keep the size

#Plotting time series
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks = "4 months",date_labels = "%Y-%m") + #breaks,
  labs(x = "Date", y = "New cases")+
  geom_line(aes(x=date,y=roll_mean),color="red")

