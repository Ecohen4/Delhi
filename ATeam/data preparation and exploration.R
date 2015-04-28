##data preparation and exploration  
##heatmap and cal-heatmap
##Author: JanetWenjuanDeng
##date:20141213

setwd("F:/recentwork/data viz/data/update/")

all.data<-read.csv("hourly2011-2013.csv")

total<-subset(all.data,all.data$discom=="total")
BRPL<-subset(all.data,all.data$discom=="BRPL")
BYPL<-subset(all.data,all.data$discom=="BYPL")
MES<-subset(all.data,all.data$discom=="MES")
NDMC<-subset(all.data,all.data$discom=="NDMC")
NDPL<-subset(all.data,all.data$discom=="NDPL")

range(BRPL$mW)
range(BYPL$mW)
range(MES$mW)
range(NDMC$mW)
range(NDPL$mW)
range(total$mW)

##this is just for exploration purpose.I found that I put four values in each grid, but the heatmap only select the fourth value to display. 
BRPL$date<-as.character(BRPL$date)
BRPL201107<-subset(BRPL,grepl("2011-07",BRPL$date))
BRPL201107$date[grep(6645.85719,BRPL201107$mW)]

BRPL20120725<-subset(BRPL,grepl("2012-07-25",BRPL$date))
BRPL20110725<-subset(BRPL,grepl("2011-07-25",BRPL$date))
##the energy use in 2012 is a bit higher than that in 2011

##Here I mainly use heatmap to show the pattern of energy use within one week.
##Patterns of energy use within one week may differ in different districts. 
##Even though the absolute value of energy use may change over the year, the pattern within one week doesn't change much across years.
#Thus, I select one year data and calculate the average energy use with dimensions of day and hour


##calculate average energy use with dimensions of day and hour in 2012

##get subset data in 2012 
BRPL2012<-subset(BRPL,grepl("2012",BRPL$date))
BYPL2012<-subset(BYPL,grepl("2012",BYPL$date))
MES2012<-subset(MES,grepl("2012",MES$date))
NDMC2012<-subset(NDMC,grepl("2012",NDMC$date))
NDPL2012<-subset(NDPL,grepl("2012",NDPL$date))
City2012<-subset(total,grepl("2012",total$date))

##Calculte the average energy use
BRPL2012.ave<-aggregate(BRPL2012$Mus,by=list(BRPL2012$day,BRPL2012$hour),FUN=mean)
names(BRPL2012.ave)<-c("day","hour","value")
write.csv(BRPL2012.ave,file='BRPL2012.csv',row.names=F)

BYPL2012.ave<-aggregate(BYPL2012$Mus,by=list(BYPL2012$day,BYPL2012$hour),FUN=mean)
names(BYPL2012.ave)<-c("day","hour","value")
write.csv(BYPL2012.ave,file='BYPL2012.csv',row.names=F)

MES2012.ave<-aggregate(MES2012$Mus,by=list(MES2012$day,MES2012$hour),FUN=mean)
names(MES2012.ave)<-c("day","hour","value")
write.csv(MES2012.ave,file='MES2012.csv',row.names=F)

NDMC2012.ave<-aggregate(NDMC2012$Mus,by=list(NDMC2012$day,NDMC2012$hour),FUN=mean)
names(NDMC2012.ave)<-c("day","hour","value")
write.csv(NDMC2012.ave,file='NDMC2012.csv',row.names=F)

NDPL2012.ave<-aggregate(NDPL2012$Mus,by=list(NDPL2012$day,NDPL2012$hour),FUN=mean)
names(NDPL2012.ave)<-c("day","hour","value")
write.csv(NDPL2012.ave,file='NDPL2012.csv',row.names=F)

City2012.ave<-aggregate(City2012$Mus,by=list(City2012$day,City2012$hour),FUN=mean)
names(City2012.ave)<-c("day","hour","value")
write.csv(City2012.ave,file='City2012.csv',row.names=F)

##Here I want to use daily aggregated data to make a cal-heatmap

##aggregate hourly data in 2012 into daily data 
City2012.daily<-aggregate(City2012$Mus,by=list(City2012$date),FUN=sum)
names(City2012.daily)<-c("date","value")
range(City2012.daily$value)
write.csv(City2012.daily,file='City2012_daily.csv',row.names=F)

min(City2012.daily$value)
max(City2012.daily$value)

City2012.daily$date[which(City2012.daily$value==min(City2012.daily$value))]
City2012.daily$date[which(City2012.daily$value==max(City2012.daily$value))]




