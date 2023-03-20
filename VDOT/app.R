#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

################################# Import Libraries #################################
library(shiny)
library(dplyr)
library(tidyr)
library(xgboost)
library(caret)
library(ggplot2)
library(lubridate)

################################# Load Model #################################
load("HourModel2.rda")    # Load saved model


################################# Shiny App ################################# 

################################# UI ################################# 
ui <- fluidPage(

    # Application title
    titlePanel("Total vehicles by Hour"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("DatesMerge",
                        "Dates:",
                        min = Sys.Date(),
                        max = Sys.Date()+14,
                        value = Sys.Date(),
                        #min = as.Date(min(predictions$Date)),
                        #max = as.Date(max(predictions$Date)),
                        #value=as.Date(max(predictions$Date)),
                        timeFormat="%Y-%m-%d")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

################################# Server #################################
server <- function(input, output) {
  
  
  ################################# Create Predictive Data #################################
  
  #Parameters
  days = 14
  #days = input$days #how many days ahead to predict (starting today)
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
  newdata <- data.frame(datetime = character(length=length(ts)),
                        Date = character(length=length(ts)),
                        Hour = character(length=length(ts)))
  newdata$datetime <- ts
  newdata$Date <- format(as.POSIXct(newdata$datetime), format = "%Y-%m-%d")
  newdata$Date <- as.POSIXct(newdata$Date, format = "%Y-%m-%d")
  newdata$Hour <- format(as.POSIXct(newdata$datetime), format = "%H") #Time to Hour
  
  newdata <- subset(newdata, select=-c(datetime))
  
  #Merge
  newdata <- left_join(newdata, data, by = c("Date" = "Full_Date"))
  
  #Remove Rows with NAs
  newdata = na.omit(newdata)
  
  #Change Column names
  newdata <- newdata %>%
    rename(
      WeekDay = Day.1)
  
  
  ################################# Make Predictions #################################
  #Make Predictions
  predictions <- subset(newdata, select=c(Date, Hour))
  newdata <- subset(newdata, select=-c(Date,snow))
  
  #Change Data Types
  newdata$Hour <- as.character(newdata$Hour)
  newdata$Month <- as.character(newdata$Month)
  newdata$Day <- as.character(newdata$Day)
  newdata$Week.Number <- as.character(newdata$Week.Number)
  newdata$Day.of.Week <- as.character(newdata$Day.of.Week)
  newdata$Holiday <- as.character(newdata$Holiday)
  newdata$covid <- as.factor(newdata$covid)
  
  newdata <- data.matrix(newdata, rownames.force = NA)
  
  predictions["TOTAL_Hour_Predict"] <- predict(model, newdata = newdata)
  
  #Turn negative numbers to 0
  predictions$TOTAL_Hour_Predict[predictions$TOTAL_Hour_Predict < 0] <- 0
  
  #Round predictions up
  predictions$TOTAL_Hour_Predict <- round(predictions$TOTAL_Hour_Predict, digits = 0)
  
  #Turn into DateTime
  predictions <- within(predictions, datetime <- as.POSIXct(paste(Date, Hour), format = "%Y-%m-%d %H"))

  output$plot <- renderPlot({
    #Add input$x for input values
    window <- predictions[predictions$Date == input$DatesMerge,] 
    ggplot(data=window, aes(x = datetime, y = TOTAL_Hour_Predict)) + 
      geom_bar(stat = "identity") +
      xlab('Time') + ylab('Number of Vehicles') +
      theme_minimal()
  })
  
}

################################# Run App ################################# 
shinyApp(ui = ui, server = server)
