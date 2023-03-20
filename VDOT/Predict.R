
#Import Libraries
library(lubridate)
library(dplyr)
library(jsonlite)
library(httr)


#Parameters
days = 30 #how many days ahead to predict (starting today)
apiKey = 'N3MWRWQ2C9R4B3M2FUNB38Q2R' #Weather Data API key from https://www.visualcrossing.com/weather/weather-data-services

#Date Range
start_date = Sys.Date()
end_date = Sys.Date()+days

#Create Date Lookup Table
df <- data.frame(Full_Date = character(length=days),
                Month = character(length=days),
                Day = character(length=days),
                Year = character(length=days),
                'Week Number' = character(length=days),
                'Day of Week' = character(length=days),
                Day.1 = character(length=days))
df$Full_Date <- seq(from = start_date, by = 'day', length.out = days)
df$Month <- month(df$Full_Date)
df$Day <- day(df$Full_Date)
df$Year <- year(df$Full_Date)
df$Week.Number <- week(df$Full_Date)
df$Day.of.Week <- wday(df$Full_Date)
df$Day.1 <- wday(df$Full_Date, label=TRUE)

#Holidays
holiday <- read.csv("Holidays.csv")
holiday$Date <- as.POSIXct(holiday$Date, format = "%m/%d/%Y")

#Take Holidays within next x days
holiday <- holiday[holiday$Date >= as.Date(start_date) & holiday$Date <= as.Date(end_date),]

#Merge
data <- left_join(df, holiday, by = c("Full_Date" = "Date"))
data$Holiday[is.na(data$Holiday)] <- 'No Holiday' #fill NA with "No Holiday"

#Weather
weather = read.csv(url(paste('https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/Jamestown/',as.Date(start_date),'/',as.Date(end_date),'?unitGroup=us&include=days&key=',apiKey,'&contentType=csv', sep="")))
weather = subset(weather, select = c(datetime,temp,precip,snow,windspeed))
weather$datetime <- as.POSIXct(weather$datetime, format = "%Y-%m-%d") #change to datetime

#Merge
data <- left_join(data, weather, by = c("Full_Date" = "datetime"))
data$snow[is.na(data$snow)] <- 0 #fill NA for snow with 0

#Create Covid Column
data$covid = rep(0,days)

#Create Hour Column
ts <- seq.POSIXt(as.POSIXct(start_date,"%Y-%m-%d"), as.POSIXct(end_date,"%Y-%m-%d"), by="hour")
datanew <- data.frame(datetime = character(length=length(ts)),
                      Date = character(length=length(ts)),
                      Hour = character(length=length(ts)))
datanew$datetime <- ts
datanew$Date <- format(as.POSIXct(datanew$datetime), format = "%Y-%m-%d")
datanew$Date <- as.POSIXct(datanew$Date, format = "%Y-%m-%d")
datanew$Hour <- format(as.POSIXct(datanew$datetime), format = "%H") #Time to Hour

datanew <- subset(datanew, select=-c(datetime))

#Merge
datanew <- left_join(datanew, data, by = c("Date" = "Full_Date"))

#Remove Rows with NAs
datanew = na.omit(datanew)

#Change Column names
datanew <- datanew %>%
  rename(
    WeekDay = Day.1)

#Crete AM/PM Rush Feature
datanew <- datanew %>%
  mutate(Rush = ifelse((Hour >= 5 & Hour <=8)|(Hour >= 15 & Hour <= 20), 1, 0))

#Export to csv
write.csv(datanew,"PredictiveData.csv")

