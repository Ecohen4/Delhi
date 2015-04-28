##goal1:aggregate quarter hourly data into hourly data, add all utility company data
##goal2:keep information of date, company, day, hour
##Delhi Electricity Hourly Data Aggregated.R 
##heatmap and cal-heatmap
##Author: JanetWenjuanDeng
##date:20141213

setwd("F:/recentwork/data viz/data/datas_source/")
data.2011.2013<-read.csv("loads2011-2013.csv")

##create new variables, indicating day and hour
data.2011.2013$day<-as.numeric(ifelse(data.2011.2013$timepoint%%96==0,data.2011.2013$timepoint%/%96,data.2011.2013$timepoint%/%96+1))
data.2011.2013$hour<-as.numeric(ifelse(data.2011.2013$timepoint%%96==0,24,(data.2011.2013$timepoint-(data.2011.2013$day-1)*96-1)%/%4+1))

##aggreage the quarterly hourly data into hourly data
hourly <- aggregate(data.2011.2013$mW, by=list(data.2011.2013$days,data.2011.2013$discom,data.2011.2013$date,data.2011.2013$day,data.2011.2013$hour), FUN=sum)
names(hourly) <- c("days", "discom", "date", "day","hour","mW")
##convert the units into MUs
hourly$mW<-hourly$mW/4000
names(hourly) <- c("days", "discom", "date", "day","hour","Mus")

##aggregate the data of five companies into citywide data
all.hourly<-aggregate(hourly$Mus,by=list(hourly$days,hourly$date,hourly$day,hourly$hour),FUN=sum)
names(all.hourly)<-c("days","date","day","hour","Mus")
all.hourly$discom<-("total")

##add the nationawide data to the dataset
all.hourly<-rbind(all.hourly,hourly)

#export the file
write.csv(all.hourly,file='F:/recentwork/data viz/data/update/hourly2011-2013.csv',row.names=F)
