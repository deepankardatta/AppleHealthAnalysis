library(dplyr)
library(ggplot2)
library(lubridate)
library(XML)

# From http://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/#4
# Script: https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d

#load apple health export.xml file
xml <- xmlParse("data/2017-10-10-export.xml")

#transform xml file to data frame - select the Record rows from the xml file
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
str(df)

#make value variable numeric
df$value <- as.numeric(as.character(df$value))
str(df)

#make endDate in a date time variable POSIXct using lubridate with eastern time zone
df$endDate <-ymd_hms(df$endDate,tz="Europe/London")
str(df)

##add in year month date dayofweek hour columns
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")
str(df)

#show steps by month by year using dplyr then graph using ggplot2
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(year,month) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by month by year
  print (n=100) %>%
  #graph data by month by year
  ggplot(aes(x=month, y=steps, fill=year)) + 
  geom_bar(position='dodge', stat='identity') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

#boxplot data by month by year
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=month, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

#summary statistics by month for 2015
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2015) %>%
  group_by(month) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75))

#boxplot data by day of week year
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=dayofweek, y=steps)) + 
  geom_boxplot(aes(fill=(year))) + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())

#summary statistics by day of week for 2015
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(dayofweek,date,year) %>%
  summarize(steps=sum(value)) %>%
  filter(year==2015) %>%
  group_by(dayofweek) %>%
  summarize(mean = round(mean(steps), 2), sd = round(sd(steps), 2), 
            median = round(median(steps), 2), max = round(max(steps), 2), 
            min = round(min(steps), 2),`25%`= quantile(steps, probs=0.25),
            `75%`= quantile(steps, probs=0.75)) %>%
  arrange(desc(median))

#heatmap day of week hour of day
df %>%
  filter(type == 'HKQuantityTypeIdentifierStepCount') %>%
  group_by(date,dayofweek,hour) %>% 
  summarize(steps=sum(value)) %>% 
  group_by(hour,dayofweek) %>% 
  summarize(steps=sum(steps)) %>% 
  arrange(desc(steps)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=dayofweek, y=hour, fill=steps)) + 
  geom_tile() + 
  scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
  theme_bw() + 
  theme(panel.grid.major = element_blank())