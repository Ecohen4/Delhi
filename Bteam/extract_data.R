rm(list=ls())
library(gdata)
library(ggplot2)

#store data on energy consumption for each company at each day within the SAME week together
dayE <- function(file){
  #read excel
  week <- read.xls(file, header = FALSE) 
  #rename column names 
  names <- c()
  for (i in 1:ncol(week)){names[i] <- as.character(week[2, i])}
  colnames(week) <- names
  #extract dates of the week
  dates <- c(as.character(week[1, 1]), as.character(week[1, 9]), as.character(week[1, 16]),
             as.character(week[1, 23]), as.character(week[1, 30]), as.character(week[1, 37]),
             as.character(week[1, 44]))
  #subset the total MUs for each day
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

#count the number of files of the same extension ".xls" in a given folder
countFiles <- function(foldername, extension){
  name = paste0("*.", extension)
  list <- list.files(path.expand(foldername), pattern = name)
  return(length(list))
}

#summarize energy consumption for each company at each day across all weeks
oneF <- function(foldername){
  setwd(foldername)
  weeks <- data.frame()  
  #!!because of loss of datasets, need to extend this to 50 later
  n <- countFiles(foldername, "xls")  
  
  if (substr(foldername, start=25, stop=29)=="10-11")
    #since no data before 2012-06-07
    for (i in 6:n+5){
      renew <- dayE(paste(as.character(i), ".xls", sep=""))
      weeks <- rbind(weeks, renew) 
    }
    #since no data after 2012-09-30
  else if (substr(foldername, start=25, stop=29)=="12-13")
  for (i in 1:27){
    renew <- dayE(paste(as.character(i), ".xls", sep=""))
    weeks <- rbind(weeks, renew)
  }
  else
    for (i in 1:n){
      renew <- dayE(paste(as.character(i), ".xls", sep=""))
      weeks <- rbind(weeks, renew)
    }
  return(weeks) 
  }  

#summary dataset for year 2010-2013
day_2010 <- oneF("~/Documents/Delhi/BTeam/10-11")

day_2011 <- oneF("~/Documents/Delhi/BTeam/11-12")

day_2012 <- oneF("~/Documents/Delhi/BTeam/12-13")

#Data summary from 2010-06-07 to 2012-9-30, data after 2012-9-30 is lost
day_total <- rbind(day_2010, day_2011, day_2012)

setwd("~/Documents/Delhi/BTeam")
save(day_total, file = "total_daily.Rdata")

