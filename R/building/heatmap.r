library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)
library(RColorBrewer)

heart_rate_data <-  ah_data_select( health_data , "HeartRate" )

date_text <- heart_rate_data$endDate - dt2
dt2 <- ymd(heart_rate_data$date , tz = "UTC" )

test <- heart_rate_data %>%
  group_by(  date , hour )  %>%
  summarise( heart_rate = mean(value) , n = n() )

ggplot( test , aes(x = hour, y = date) ) +
  geom_raster( aes(fill = heart_rate) ) +
  scale_fill_gradientn(guide = "legend", colours = brewer.pal(n = 9, name = "Oranges"))

mean_heart_rate <- mean(test$heart_rate)
start_date <- min(test$date)
end_date <- max(test$date)

for(i in start_date:end_date) {
  print(test$date)
}


test2 <- heart_rate_data %>% spread(type, value) %>%
  mutate(`HeartRate` = ifelse(is.na(`HeartRate`), median(`HeartRate`, na.rm = T), `HeartRate`))

