#Importing uber data set
uber <- read.csv("Uber Request Data.csv", header = T, quote = "\"", fill = T, comment.char = "" , stringsAsFactors = F)


#loading required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(chron)

#DATA UNDERSTANDING AND DATA CLEANING
#Checking for duplicate request ID's
sum(duplicated(uber$Request.id))    #no duplicates

#Checking NA's
sum(is.na(uber$Request.id))
sum(is.na(uber$Pickup.point))
sum(is.na(uber$Driver.id))   #NA's are present since No cars were found.
sum(is.na(uber$Status))
sum(is.na(uber$Request.timestamp))
sum(is.na(uber$Drop.timestamp))   #NA's for no drops, since no cars available and for cancelled trips

#structure of data set
str(uber)

#coverting pickup point and status columns into factor
uber$Pickup.point <- as.factor(uber$Pickup.point)
uber$Status <- as.factor(uber$Status)

#converting date to common delimiter and formatting data & time column
uber$Request.timestamp <- gsub("-", "/", uber$Request.timestamp)
uber$Request.timestamp <- as.POSIXct(uber$Request.timestamp, format = "%d/%m/%Y %H:%M")

uber$Drop.timestamp <- gsub("-", "/", uber$Drop.timestamp)
uber$Drop.timestamp <- as.POSIXct(uber$Drop.timestamp, format = "%d/%m/%Y %H:%M")

#Creating hour and date column for date and time analysis and coverting integers to factor
uber$Request_date <- as.factor(format(uber$Request.timestamp, "%d"))
uber$Request_hour <- as.factor(format(uber$Request.timestamp, "%H"))
uber$Drop_hour <- as.factor(format(uber$Drop.timestamp, "%H"))
summary(uber)
#summary shows number of requests from airport and city, counts of all status, count of status on each day and count of requests at each hour

#EDA (Univariate and segmented analysis): Plotting graphs to analyse supply demand gap
ggplot(uber, aes(Status, fill = Status)) + geom_bar()
ggplot(uber, aes(Pickup.point, fill = Pickup.point)) + geom_bar()
ggplot(uber, aes(Pickup.point, fill = Status)) + geom_bar(position = "dodge")
ggplot(uber, aes(Request_hour, fill = Status)) + geom_bar() + facet_wrap(~uber$Pickup.point, nrow = 1, ncol = 2)
ggplot(uber, aes(Request_hour, fill = Status)) + geom_bar() + facet_wrap(~uber$Request_date, nrow = 5, ncol = 2)
ggplot(uber, aes(Request_hour, fill = Pickup.point)) + geom_bar() + facet_wrap(~uber$Request_date, nrow = 5, ncol = 2)
ggplot(uber, aes(Drop_hour, fill = Status)) + geom_bar()  #NA values show that ride either Cancelled or No cars available
#Assumption: Pickup point - Airport means car will move from airport to city; Pickup point - City means car will move from city to airport


#Creating time_interval column from request hour with following categories: Late Night, Early Morning, Afternoon/Early evening, Late Evening/Night
uber$Request_hour <- as.numeric(uber$Request_hour)
uber$time_interval <- ifelse(uber$Request_hour <= 4, "Late Night", 
                             ifelse(uber$Request_hour <= 10, "Morning", 
                                    ifelse(uber$Request_hour <= 16, "Afternoon/Early evening", "Late evening/Night")))
uber$time_interval <- as.factor(uber$time_interval)
ggplot(uber, aes(Pickup.point, fill = Status)) + geom_bar(position = "stack") + facet_wrap(~uber$time_interval, nrow = 2, ncol = 2)
#This plot shows time interval to focus are morning and Late evening/Night since these have more number of requests


#Morning analysis, filtering morning time interval requests
uber_morning <- filter(uber, time_interval =="Morning")
ggplot(uber_morning, aes(Pickup.point, fill = Status)) + geom_bar()
#In morning time, most of the rides are cancelled at city

#number of requests status at each point in morning (Supply and demand gap)
morning_airport <- nrow(filter(uber_morning, Pickup.point == "Airport"))
morning_city <- nrow(filter(uber_morning, Pickup.point == "City"))
morning_cancelled_airport <- nrow(filter(uber_morning, Pickup.point == "Airport" & Status == "Cancelled"))
morning_cancelled_city <- nrow(filter(uber_morning, Pickup.point == "City" & Status == "Cancelled"))
morning_no.cars_airport <- nrow(filter(uber_morning, Pickup.point == "Airport" & Status == "No Cars Available"))
morning_no.cars_city <- nrow(filter(uber_morning, Pickup.point == "City" & Status == "No Cars Available"))
morning_completed_airport <- nrow(filter(uber_morning, Pickup.point == "Airport" & Status == "Trip Completed"))
morning_completed_city <- nrow(filter(uber_morning, Pickup.point == "City" & Status == "Trip Completed")) 


#Morning problem at city
uber_morning_problem <- filter(uber_morning, Pickup.point == "City")
ggplot(uber_morning_problem, aes(Pickup.point, fill = Status)) + geom_bar()
tripcompleted_morning_city_percent <- morning_completed_city / morning_city * 100
tripcancelled_morning_city_percent <- morning_cancelled_city / morning_city * 100
tripnocars_morning_city_percent <- morning_no.cars_city / morning_city * 100
#Shows almost 50% of rides are cancelled, 23% rides shows no cars available and 25% rides are completed in morning at city are cancelled

#Evening analysis, filtering late night/evenning time interval requests
uber_evening <- filter(uber, time_interval =="Late evening/Night")
ggplot(uber_evening, aes(Pickup.point, fill = Status)) + geom_bar()
#In evening time, many times no cars are available

#number of requests status at each point in evening(supply and demand gap)
evening_airport <- nrow(filter(uber_evening, Pickup.point == "Airport"))
evening_city <- nrow(filter(uber_evening, Pickup.point == "City"))
evening_cancelled_airport <- nrow(filter(uber_evening, Pickup.point == "Airport" & Status == "Cancelled"))
evening_cancelled_city <- nrow(filter(uber_evening, Pickup.point == "City" & Status == "Cancelled"))
evening_no.cars_airport <- nrow(filter(uber_evening, Pickup.point == "Airport" & Status == "No Cars Available"))
evening_no.cars_city <- nrow(filter(uber_evening, Pickup.point == "City" & Status == "No Cars Available"))
evening_completed_airport <- nrow(filter(uber_evening, Pickup.point == "Airport" & Status == "Trip Completed"))
evening_completed_city <- nrow(filter(uber_evening, Pickup.point == "City" & Status == "Trip Completed")) 

#Evening problem at airport
uber_evening_problem <- filter(uber_evening, Pickup.point == "Airport")
ggplot(uber_evening_problem, aes(Pickup.point, fill = Status)) + geom_bar()
tripcompleted_evening_airport_percent <- evening_completed_airport / evening_airport * 100
tripcancelled_evening_airport_percent <- evening_cancelled_airport / evening_airport * 100
tripno.cars_evening_airport_percent <- evening_no.cars_airport / evening_airport * 100
#Shows almost almost 70% requests are not successful because of no cars available


#Exporting
write.csv(uber, "uber.csv", row.names = F)
