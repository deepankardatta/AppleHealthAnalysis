require(XML)
require(dplyr)
require(ggplot2)
require(tidyr)

# FROM http://rpubs.com/Ranthony__/visualizing-iphone-health-app-data-in-R

#parse XML file & convert to list 
data<-xmlParse("data/2017-10-10-export.xml")
xml_data<-xmlToList(data)

#Example of a record entry
xml_data[30000]

#unlist xml_data to access individual elements by name 
#Is there a more efficient way to do this?
xml_unlist<-unlist(xml_data)

#extract data of interest: type, unit, value, date
#is there a cleaner way to obtain the same result? 
Record.type<-as.vector(xml_unlist[grep("Record.type|Record..attrs.type",names(xml_unlist))])
Record.unit<-as.vector(xml_unlist[grep("Record.unit|Record..attrs.unit",names(xml_unlist))])
Record.value<-as.vector(xml_unlist[grep("Record.value|Record..attrs.value",names(xml_unlist))])
Record.creationDate<-as.vector(xml_unlist[grep("Record.creationDate|Record..attrs.creationDate",names(xml_unlist))])

#combine data of interest into a single data.frame 
healthData<-data.frame(cbind(Record.creationDate,Record.type,Record.unit,Record.value),stringsAsFactors=F)

#change value column data type to numeric
healthData$Record.value<-as.numeric(healthData$Record.value)

#separate date column into its constituents, delimited by spaces
healthData<-healthData %>% separate(Record.creationDate,c('date','time','misc'),sep=" ")

#further separate date into year,month,day columns 
healthData<-healthData %>% separate(date,c('year','month','day'),sep="-")

#make Record.type column more readable 
healthData$Record.type<-gsub('HKQuantityTypeIdentifier',"",healthData$Record.type)

#summarise values by monthly totals 
healthData_summary<-arrange(summarise(group_by(healthData,Record.type,year,month),sum=sum(Record.value)),desc(year)) %>% data.frame()

#view structure of data.frame
str(subset(healthData,Record.type=='DistanceWalkingRunning'))

ggplot(subset(healthData_summary,Record.type=="DistanceWalkingRunning"),aes(x=Record.type,y=sum,fill=year))+geom_bar(stat="identity")+facet_grid(.~month)+geom_text(aes(label=round(sum,1)),position=position_dodge(width=0.9),vjust=-0.25) + theme(axis.text.x=element_blank()) + labs(list(title="Total Running+Walking Distance per Month for 2015-2016",x="Walking+Running",y="Total Distance (mi)"))
