setwd("~/github/Delhi")
library(plyr)
SLDC <- read.csv("Delhi-SLDC-data-15min-master.csv")

# Seperate Date into yr-month-day
ymd<-strsplit(as.character(SLDC$date),"/")
SLDC$year<-laply(ymd, '[[', 3) #assign the list of years to an array called SLDC$year
SLDC$month<-laply(ymd, '[[', 1)
SLDC$day<-laply(ymd, '[[', 2)

SLDC$year<-as.factor(SLDC$year)
SLDC$month<-as.factor(SLDC$month)
SLDC$day<-as.factor(SLDC$day)

##
install.packages("rjson")
require(rjson)
daily <- fromJSON(file="Delhi-Daily-GWh.json")
daily <- lapply(daily, function(x) round(as.numeric(x), digits=3))
daily.json <- toJSON(daily)




