rm(list=ls())
setwd("/Users/yuelong/Documents/DataV/BTeam/seaonsal_changes")

BRPL_fall <- read.csv("BRPL_fall.csv")
BRPL_monsoon <- read.csv("BRPL_monsoon.csv")
BRPL_summer <- read.csv("BRPL_summer.csv")
BRPL_winter <- read.csv("BRPL_winter.csv")

BYPL_fall <- read.csv("BYPL_fall.csv")
BYPL_monsoon <- read.csv("BYPL_monsoon.csv")
BYPL_summer <- read.csv("BYPL_summer.csv")
BYPL_winter <- read.csv("BYPL_winter.csv")

NDPL_fall <- read.csv("NDPL_fall.csv")
NDPL_monsoon <- read.csv("NDPL_monsoon.csv")
NDPL_summer <- read.csv("NDPL_summer.csv")
NDPL_winter <- read.csv("NDPL_winter.csv")

NDMC_fall <- read.csv("NDMC_fall.csv")
NDMC_monsoon <- read.csv("NDMC_monsoon.csv")
NDMC_summer <- read.csv("NDMC_summer.csv")
NDMC_winter <- read.csv("NDMC_winter.csv")

MES_fall <- read.csv("MES_fall.csv")
MES_monsoon <- read.csv("MES_monsoon.csv")
MES_summer <- read.csv("MES_summer.csv")
MES_winter <- read.csv("MES_winter.csv")

colnames(BRPL_fall) <- c("Time", "fall_in_Megawatts")
colnames(BYPL_fall) <- c("Time", "fall_in_Megawatts")
colnames(NDPL_fall) <- c("Time", "fall_in_Megawatts")
colnames(NDMC_fall) <- c("Time", "fall_in_Megawatts")
colnames(MES_fall) <- c("Time", "fall_in_Megawatts")

colnames(BRPL_winter) <- c("Time", "winter_in_Megawatts")
colnames(BYPL_winter) <- c("Time", "winter_in_Megawatts")
colnames(NDPL_winter) <- c("Time", "winter_in_Megawatts")
colnames(NDMC_winter) <- c("Time", "winter_in_Megawatts")
colnames(MES_winter) <- c("Time", "winter_in_Megawatts")

colnames(BRPL_monsoon) <- c("Time", "monsoon_in_Megawatts")
colnames(BYPL_monsoon) <- c("Time", "monsoon_in_Megawatts")
colnames(NDPL_monsoon) <- c("Time", "monsoon_in_Megawatts")
colnames(NDMC_monsoon) <- c("Time", "monsoon_in_Megawatts")
colnames(MES_monsoon) <- c("Time", "monsoon_in_Megawatts")

colnames(BRPL_summer) <- c("Time", "summer_in_Megawatts")
colnames(BYPL_summer) <- c("Time", "summer_in_Megawatts")
colnames(NDPL_summer) <- c("Time", "summer_in_Megawatts")
colnames(NDMC_summer) <- c("Time", "summer_in_Megawatts")
colnames(MES_summer) <- c("Time", "summer_in_Megawatts")

BRPL_season <- cbind(BRPL_winter, BRPL_summer, BRPL_monsoon, BRPL_fall)
BRPL_season <- with (BRPL_season, BRPL_season[,c(1,2,4,6,8)])

BYPL_season <- cbind(BYPL_winter, BYPL_summer, BYPL_monsoon, BYPL_fall)
BYPL_season <- with (BYPL_season, BYPL_season[,c(1,2,4,6,8)])

NDPL_season <- cbind(NDPL_winter, NDPL_summer, NDPL_monsoon, NDPL_fall)
NDPL_season <- with (NDPL_season, NDPL_season[,c(1,2,4,6,8)])

NDMC_season <- cbind(NDMC_winter, NDMC_summer, NDMC_monsoon, NDMC_fall)
NDMC_season <- with (NDMC_season, NDMC_season[,c(1,2,4,6,8)])

MES_season <- cbind(MES_winter, MES_summer, MES_monsoon, MES_fall)
MES_season <- with (MES_season, MES_season[,c(1,2,4,6,8)])

#compare companies by season
winter <- cbind(BRPL_winter, BYPL_winter, NDPL_winter, NDMC_winter, MES_winter)
winter <- with (winter, winter[, c(1,2,4,6,8,10)])
colnames(winter) <- c("Time", "BRPL_winter", "BYPL_winter", "NDPL_winter", "NDMC_winter", "MES_winter")

summer <- cbind(BRPL_summer, BYPL_summer, NDPL_summer, NDMC_summer, MES_summer)
summer <- with (summer, summer[, c(1,2,4,6,8,10)])
colnames(summer) <- c("Time", "BRPL_summer", "BYPL_summer", "NDPL_summer", "NDMC_summer", "MES_summer")

monsoon <- cbind(BRPL_monsoon, BYPL_monsoon, NDPL_monsoon, NDMC_monsoon, MES_monsoon)
monsoon <- with (monsoon, monsoon[, c(1,2,4,6,8,10)])
colnames(monsoon) <- c("Time", "BRPL_monsoon", "BYPL_monsoon", "NDPL_monsoon", "NDMC_monsoon", "MES_monsoon")

fall <- cbind(BRPL_fall, BYPL_fall, NDPL_fall, NDMC_fall, MES_fall)
fall <- with (fall, fall[, c(1,2,4,6,8,10)])
colnames(fall) <- c("Time", "BRPL_fall", "BYPL_fall", "NDPL_fall", "NDMC_fall", "MES_fall")

library(gdata)
write.csv(winter, file = "winter.csv")
write.csv(fall, file = "fall.csv")
write.csv(monsoon, file = "monsoon.csv")
write.csv(summer, file = "summer.csv")

library(gdata)
write.csv(BRPL_season, file = "BRPL_season.csv")
write.csv(BYPL_season, file = "BYPL_season.csv")
write.csv(NDPL_season, file = "NDPL_season.csv")
write.csv(NDMC_season, file = "NDMC_season.csv")
write.csv(MES_season, file = "MES_season.csv")




rm(list=ls())
setwd("/Users/yuelong/Documents/DataV/BTeam/seaonsal_changes")

BRPL <- read.csv("BRPL_season.csv", header = FALSE)
BRPL <- BRPL[,-1]
colnames(BRPL)<- lapply(BRPL[1,], as.character)
BRPL <- BRPL[-1,]
BRPL <- BRPL[1:96,]

BRPL_long <- reshape(BRPL, 
             varying = c("winter_in_Megawatts", "summer_in_Megawatts", "fall_in_Megawatts", "monsoon_in_Megawatts"), 
             v.names = "consumption",
             timevar = "season", 
             times = c("winter", "summer", "fall", "monsoon"), 
             new.row.names = 1:1000,             
             direction = "long")

BRPL_long <- data.frame(lapply(BRPL_long, as.character), stringsAsFactors = FALSE)

BRPL_long <- transform(BRPL_long, consumption = as.numeric(consumption))
write.csv(BRPL_long, file = "BRPL_long.csv")

#BYPL
BYPL <- read.csv("BYPL_season.csv", header = FALSE)
colnames(BYPL)<- lapply(BYPL[1,], as.character)
BYPL <- BYPL[-1,]
BYPL <- BYPL[1:96,]

BYPL_long <- reshape(BYPL, 
                     varying = c("winter_in_Megawatts", "summer_in_Megawatts", "fall_in_Megawatts", "monsoon_in_Megawatts"), 
                     v.names = "consumption",
                     timevar = "season", 
                     times = c("winter", "summer", "fall", "monsoon"), 
                     new.row.names = 1:1000,             
                     direction = "long")

BYPL_long <- data.frame(lapply(BYPL_long, as.character), stringsAsFactors = FALSE)

BYPL_long <- transform(BYPL_long, consumption = as.numeric(consumption))
write.csv(BYPL_long, file = "BYPL_long.csv")


#NDPL
NDPL <- read.csv("NDPL_season.csv", header = FALSE)
NDPL <- NDPL[,-1]
colnames(NDPL)<- lapply(NDPL[1,], as.character)
NDPL <- NDPL[-1,]
NDPL <- NDPL[1:96,]

NDPL_long <- reshape(NDPL, 
                     varying = c("winter_in_Megawatts", "summer_in_Megawatts", "fall_in_Megawatts", "monsoon_in_Megawatts"), 
                     v.names = "consumption",
                     timevar = "season", 
                     times = c("winter", "summer", "fall", "monsoon"), 
                     new.row.names = 1:1000,             
                     direction = "long")

NDPL_long <- data.frame(lapply(NDPL_long, as.character), stringsAsFactors = FALSE)

NDPL_long <- transform(NDPL_long, consumption = as.numeric(consumption))
write.csv(NDPL_long, file = "NDPL_long.csv")


#NDMC
NDMC <- read.csv("NDMC_season.csv", header = FALSE)
colnames(NDMC)<- lapply(NDMC[1,], as.character)
NDMC <- NDMC[-1,]
NDMC <- NDMC[1:96,]

NDMC_long <- reshape(NDMC, 
                     varying = c("winter_in_Megawatts", "summer_in_Megawatts", "fall_in_Megawatts", "monsoon_in_Megawatts"), 
                     v.names = "consumption",
                     timevar = "season", 
                     times = c("winter", "summer", "fall", "monsoon"), 
                     new.row.names = 1:1000,             
                     direction = "long")

NDMC_long <- data.frame(lapply(NDMC_long, as.character), stringsAsFactors = FALSE)

NDMC_long <- transform(NDMC_long, consumption = as.numeric(consumption))
write.csv(NDMC_long, file = "NDMC_long.csv")


#MES
MES <- read.csv("MES_season.csv", header = FALSE)
colnames(MES)<- lapply(MES[1,], as.character)
MES <- MES[-1,]
MES <- MES[1:96,]

MES_long <- reshape(MES, 
                     varying = c("winter_in_Megawatts", "summer_in_Megawatts", "fall_in_Megawatts", "monsoon_in_Megawatts"), 
                     v.names = "consumption",
                     timevar = "season", 
                     times = c("winter", "summer", "fall", "monsoon"), 
                     new.row.names = 1:1000,             
                     direction = "long")

MES_long <- data.frame(lapply(MES_long, as.character), stringsAsFactors = FALSE)

MES_long <- transform(MES_long, consumption = as.numeric(consumption))
write.csv(MES_long, file = "MES_long.csv")
