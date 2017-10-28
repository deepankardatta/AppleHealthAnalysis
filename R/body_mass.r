# USE THE DATA EXTRACT FUNCTION FIRST

#boxplot data by month by year
health_data %>%
  filter(type == 'HKQuantityTypeIdentifierBodyMassIndex') %>%
  group_by(date,month,year) %>%
  summarize(steps=sum(value)) %>%
  #print table steps by date by month by year
  print (n=100) %>%
  ggplot(aes(x=month, y=steps)) + 
  geom_boxplot() + 
  scale_fill_brewer() +
  theme_bw() +  
  theme(panel.grid.major = element_blank())
