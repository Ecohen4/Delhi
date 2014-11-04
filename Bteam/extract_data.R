rm(list=ls())
library(gdata)
setwd("~/Documents/Delhi/BTeam/Actuals1213")

#function to put energy consumption for each company at each day within the SAME week together
dayE <- function(file){
  #read excel
  week <- read.xls(file, header = FALSE) 
  #rename column names 
  names <- c()
  for (i in 1:56){names[i] <- as.character(week[2, i])}
  colnames(week) <- names
  #extract dates of the week
  dates <- c(as.character(week[1, 1]), as.character(week[1, 9]), as.character(week[1, 16]),
             as.character(week[1, 23]), as.character(week[1, 30]), as.character(week[1, 37]),
             as.character(week[1, 44]))
  #subset the last row for each day and combine them together
  daily_total <- week[99, ]
  Mon <- daily_total[,c(2,3,4,5,6)]
  Tue <- daily_total[,c(9,10,11,12,13)]
  Wed <- daily_total[,c(16,17,18,19,20)]
  Thu <- daily_total[,c(23,24,25,26,27)]
  Fri <- daily_total[,c(30,31,32,33,34)]
  Sat <- daily_total[,c(37,38,39,40,41)]
  Sun <- daily_total[,c(44,45,46,47,48)]
  total_each_day <- rbind(Mon,Tue,Wed,Thu,Fri,Sat,Sun)
  colnames(total_each_day) <- c("BRPL", "BYPL", "NDPL", "NDMC", "MES") 
  rownames(total_each_day) <- dates
  return (total_each_day)
}

#function to summarize energy consumption for each company at each day across all weeks in 2012-2013
oneF <- function(){
  weeks <- data.frame()  
  #!!because of loss of datasets, need to extend this to 50 later
  for (i in 1:27){
    renew <- dayE(paste("ACTUAL", as.character(i), ".xls", sep=""))
    weeks <- rbind(weeks, renew)
  }  
  return(weeks)
}

#summary dataset
oneF()

