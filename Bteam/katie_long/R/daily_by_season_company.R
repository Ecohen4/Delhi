rm(list=ls())
library(gdata)
library(ggplot2)

#count the number of files of the same extension ".xls" in a given folder
countFiles <- function(foldername, extension){
  name = paste0("*.", extension)
  list <- list.files(path.expand(foldername), pattern = name)
  return(length(list))
}

weekly_by_company <- function(file, company){
  df1 <- read.xls(file)
  df1 <- df1[,1:48]
  df1 <-df1[,colSums(is.na(df1)) != nrow(df1)]
  week_date <- c(colnames(df1)[1],
                 colnames(df1)[7],
                 colnames(df1)[12], 
                 colnames(df1)[17], 
                 colnames(df1)[22], 
                 colnames(df1)[27], 
                 colnames(df1)[32])
  rownames(df1) <- df1[,1]  
  for (i in 1:ncol(df1)){
    colnames(df1)[i] <- as.character(df1[1,i])       
  }
  col_num1 <- c()
  col_num2 <- c()
  col_num3 <- c()
  col_num4 <- c()
  col_num5 <- c()
  
  for (i in 1:ncol(df1)){
    if (colnames(df1)[i]=="BRPL")
      col_num1 <- c(col_num1,i)
    else if (colnames(df1)[i]=="BYPL")
      col_num2 <- c(col_num2,i)
    else if (colnames(df1)[i]=="NDPL")
      col_num3 <- c(col_num3,i)
    else if (colnames(df1)[i]=="NDMC")
      col_num4 <- c(col_num4,i)
    else if (colnames(df1)[i]=="MES")
      col_num5 <- c(col_num5,i)      
  }
  
  if (company=="BRPL")
    result <- df1[col_num1]
  else if (company=="BYPL")
    result <- df1[col_num2]
  else if (company=="NDPL")
    result <- df1[col_num3]
  else if (company=="NDMC")
    result <- df1[col_num4]
  else if (company == "MES")
    result <- df1[col_num5]
  
  colnames(result) <- week_date
  result <- result[-1,]
  return(result)
}

#summarize energy consumption for each company at each day across all weeks
utility <- function(foldername, company){
  setwd(foldername)
  weeks <- data.frame()  
  n <- countFiles(foldername, "xls")  
  
      if (substr(foldername, start=25, stop=29)=="10-11")
        #since no data before 2012-06-07
        for (i in 6:n+5){
          renew <- weekly_by_company(paste(as.character(i), ".xls", sep=""), company)
          weeks <- c(weeks,renew)
        }
      else if (substr(foldername, start=25, stop=29)=="12-13"){
        for (i in 1:50){
          if ((i>=1) & (i<=27)){
            renew <- weekly_by_company(paste(as.character(i), ".xls", sep=""), company)
            weeks <- c(weeks, renew)
          }
          else
          {
            renew <- weekly_by_company(paste(as.character(i), "-1.xls", sep=""), company)
            weeks <- c(weeks,renew) 
          }
        }  
      }
      else{
        for (i in 1:n){
          renew <- weekly_by_company(paste(as.character(i), ".xls", sep=""), company)
          weeks <- c(weeks, renew)
        }
      }
  
  weeks <- as.data.frame(weeks, stringAsFactor = FALSE)
  rownames(weeks) <- rownames(weekly_by_company("20.xls", "BRPL"))
  return(weeks) 
}  

summary <- function(company){
  a <- utility("~/Documents/Delhi/BTeam/10-11", company)
  b <- utility("~/Documents/Delhi/BTeam/11-12", company)
  c <- utility("~/Documents/Delhi/BTeam/12-13", company)
  
  result <- merge(a, b, by = "row.names")
  rownames(result) <- result[,1]
  result <- result[,names(result)!="Row.names"]
  result <- merge(result, c, by = "row.names")
  rownames(result) <- result[,1]
  result <- result[,names(result)!="Row.names"]
  
  return(result)
}



separate_by_season <- function(company, season){
        a<- summary(company)
        winter <- c()
        summer <- c()
        monsoon <- c()
        fall <- c()

       for (i in 1:ncol(a)){
        if (as.numeric(substr(colnames(a)[i], start=7, stop = 8))<=3 || as.numeric(substr(colnames(a)[i], start=7, stop = 8))==12)
          winter <- c(winter, i)
        else if (as.numeric(substr(colnames(a)[i], start=7, stop = 8))<=6 && as.numeric(substr(colnames(a)[i], start=7, stop = 8))>=4)
          summer <- c(summer, i)
        else if (as.numeric(substr(colnames(a)[i], start=7, stop = 8))<=9 && as.numeric(substr(colnames(a)[i], start=7, stop = 8))>=7)
          monsoon <- c(monsoon, i)
        else 
          fall <- c(fall, i)  
        }

       if (season =="winter")
        return (as.data.frame(a[winter]))
       else if (season == "summer")
        return (as.data.frame(a[summer]))
       else if (season == "monsoon")
        return (as.data.frame(a[monsoon]))
       else 
        return (as.data.frame(a[fall]))
}
  
daily_summary_by_season <- function (company, season){
  a <- separate_by_season(company, season)
  oneday_by_company_by_season <- data.frame(matrix(NA, nrow = 97, ncol=1))
  
  for (i in 1:97){
    avEnergy <- c()
    for (j in 1:ncol(a)){     
      avEnergy<- c(avEnergy, as.numeric(paste0(a[i,j])))
    }
    oneday_by_company_by_season[i,1]<- mean(avEnergy)
  } 

rownames(oneday_by_company_by_season) <- rownames(a)
colnames(oneday_by_company_by_season) <- paste("Average Energy Consumption in MUs in", season, "from", company) 
return (oneday_by_company_by_season)
}

BRPL_winter <- daily_summary_by_season("BRPL", "winter")
BRPL_summer <- daily_summary_by_season("BRPL", "summer")
BRPL_monsoon <- daily_summary_by_season("BRPL", "monsoon")
BRPL_fall <- daily_summary_by_season("BRPL", "fall")

BYPL_winter <- daily_summary_by_season("BYPL", "winter")
BYPL_summer <- daily_summary_by_season("BYPL", "summer")
BYPL_monsoon <- daily_summary_by_season("BYPL", "monsoon")
BYPL_fall <- daily_summary_by_season("BYPL", "fall")

NDPL_winter <- daily_summary_by_season("NDPL", "winter")
NDPL_summer <- daily_summary_by_season("NDPL", "summer")
NDPL_monsoon <- daily_summary_by_season("NDPL", "monsoon")
NDPL_fall <- daily_summary_by_season("NDPL", "fall")

NDMC_winter <- daily_summary_by_season("NDMC", "winter")
NDMC_summer <- daily_summary_by_season("NDMC", "summer")
NDMC_monsoon <- daily_summary_by_season("NDMC", "monsoon")
NDMC_fall <- daily_summary_by_season("NDMC", "fall")

MES_winter <- daily_summary_by_season("MES", "winter")
MES_summer <- daily_summary_by_season("MES", "summer")
MES_monsoon <- daily_summary_by_season("MES", "monsoon")
MES_fall <- daily_summary_by_season("MES", "fall")

library(utils)
setwd("/Users/yuelong/Documents/DataV/BTeam/seaonsal_changes")

write.csv(BRPL_winter, file = "BRPL_winter.csv")
write.csv(BRPL_summer, file = "BRPL_summer.csv")
write.csv(BRPL_monsoon, file = "BRPL_monsoon.csv")
write.csv(BRPL_fall, file = "BRPL_fall.csv")

write.csv(BYPL_winter, file = "BYPL_winter.csv")
write.csv(BYPL_summer, file = "BYPL_summer.csv")
write.csv(BYPL_monsoon, file = "BYPL_monsoon.csv")
write.csv(BYPL_fall, file = "BYPL_fall.csv")

write.csv(NDPL_winter, file = "NDPL_winter.csv")
write.csv(NDPL_summer, file = "NDPL_summer.csv")
write.csv(NDPL_monsoon, file = "NDPL_monsoon.csv")
write.csv(NDPL_fall, file = "NDPL_fall.csv")

write.csv(NDMC_winter, file = "NDMC_winter.csv")
write.csv(NDMC_summer, file = "NDMC_summer.csv")
write.csv(NDMC_monsoon, file = "NDMC_monsoon.csv")
write.csv(NDMC_fall, file = "NDMC_fall.csv")

write.csv(MES_winter, file = "MES_winter.csv")
write.csv(MES_summer, file = "MES_summer.csv")
write.csv(MES_monsoon, file = "MES_monsoon.csv")
write.csv(MES_fall, file = "MES_fall.csv")




