library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)
library(tidyr)

# From http://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/#4
# Script: https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
# Alternate method: http://rpubs.com/Ranthony__/visualizing-iphone-health-app-data-in-R

# Load exported apple health XML file
xml_health_data <- xmlParse("data/2017-10-10-export.xml")

# Transform xml file to data frame - select the Record rows from the xml file
health_data <- XML:::xmlAttrsToDataFrame(xml_health_data["//Record"])

# Make the 'value' variable numeric
health_data$value <- as.numeric(as.character(health_data$value))

# Make endDate in a date time variable POSIXct using lubridate with London time zone
health_data$endDate <-ymd_hms(health_data$endDate,tz="Europe/London")

# add in columns for: month, year, date, day of week, hour
health_data$month<-format(health_data$endDate,"%m")
health_data$year<-format(health_data$endDate,"%Y")
health_data$date<-format(health_data$endDate,"%Y-%m-%d")
health_data$dayofweek <-wday(health_data$endDate, label=TRUE, abbr=FALSE)
health_data$hour <-format(health_data$endDate,"%H")

# Clean up the type identifier so it is easier to purge
health_data$type <- gsub('HKQuantityTypeIdentifier', "" , health_data$type )
health_data$type <- gsub('HKCategoryTypeIdentifier', "" , health_data$type )

health_data_summary <- arrange(summarise(group_by(health_data,type,year,month,unit),sum=sum(value)),desc(year)) %>% data.frame()

ggplot(subset( health_data_summary , type=="DistanceWalkingRunning"), aes(x=type,y=sum,fill=year)) +
  geom_bar(stat="identity") +
  facet_grid(.~month) +
  geom_text(aes(label=round(sum,1)),position=position_dodge(width=0.9),vjust=-0.25) +
  theme(axis.text.x=element_blank()) +
  labs(list(title="Total Running & Walking Distance per Month", x="Walking & Running", y="Total Distance"))

hds <- arrange(summarise(group_by(health_data,type,year,month,unit),sum=sum(value)),desc(type,year)) %>% data.frame()


temp1 <- subset( health_data_summary , type=="DistanceWalkingRunning")
summary(temp1$unit)



