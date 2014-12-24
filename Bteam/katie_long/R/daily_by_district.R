rm(list = ls())
setwd("~/Documents/DataV/BTeam/seasonal_changes")
fall <- read.csv("fall.csv")
winter <- read.csv("winter.csv")
monsoon <- read.csv("monsoon.csv")
summer <- read.csv("summer.csv")

colnames(fall) <- c("X", "Time", "BRPL", "BYPL", "NDPL", "NDMC", "MES")
colnames(winter) <- c("X", "Time", "BRPL", "BYPL", "NDPL", "NDMC", "MES")
colnames(monsoon) <- c("X", "Time", "BRPL", "BYPL", "NDPL", "NDMC", "MES")
colnames(summer) <- c("X", "Time", "BRPL", "BYPL", "NDPL", "NDMC", "MES")

#for each district, we set the proportion of energy supply coming from different companies according to the map
#note that the numbers below are just for testing
a <- c(1/4, 1/4, 1/4, 1/4)#BYPL:4
b <- c(1/3, 1/3, 1/3)#NDPL:3
c <- c(1/3, 1/3, 1/3)#BRPL:3

#fall
fall$Central <- with(fall, a[1]*BYPL)
fall$East <- with(fall, a[2]*BYPL)
fall$NewD <- fall$NDMC
fall$North <- with(fall, b[1]*NDPL + a[3]*BYPL)
fall$NorthE <- with(fall, a[4]*BYPL)
fall$NorthW <- with(fall, b[2]*NDPL)
fall$South <- with(fall, c[1]*BRPL)
fall$SouthW <- with(fall, c[2]*BRPL + MES)
fall$West <- with(fall, c[3]*BRPL + b[3]*NDPL)

fall_dist <- fall[, c(2,8:16)] 

#winter
winter$Central <- with(winter, a[1]*BYPL)
winter$East <- with(winter, a[2]*BYPL)
winter$NewD <- winter$NDMC
winter$North <- with(winter, b[1]*NDPL + a[3]*BYPL)
winter$NorthE <- with(winter, a[4]*BYPL)
winter$NorthW <- with(winter, b[2]*NDPL)
winter$South <- with(winter, c[1]*BRPL)
winter$SouthW <- with(winter, c[2]*BRPL + MES)
winter$West <- with(winter, c[3]*BRPL + b[3]*NDPL)

winter_dist <- winter[, c(2,8:16)] 

#monsoon
monsoon$Central <- with(monsoon, a[1]*BYPL)
monsoon$East <- with(monsoon, a[2]*BYPL)
monsoon$NewD <- monsoon$NDMC
monsoon$North <- with(monsoon, b[1]*NDPL + a[3]*BYPL)
monsoon$NorthE <- with(monsoon, a[4]*BYPL)
monsoon$NorthW <- with(monsoon, b[2]*NDPL)
monsoon$South <- with(monsoon, c[1]*BRPL)
monsoon$SouthW <- with(monsoon, c[2]*BRPL + MES)
monsoon$West <- with(monsoon, c[3]*BRPL + b[3]*NDPL)

monsoon_dist <- monsoon[, c(2,8:16)] 

#summer
summer$Central <- with(summer, a[1]*BYPL)
summer$East <- with(summer, a[2]*BYPL)
summer$NewD <- summer$NDMC
summer$North <- with(summer, b[1]*NDPL + a[3]*BYPL)
summer$NorthE <- with(summer, a[4]*BYPL)
summer$NorthW <- with(summer, b[2]*NDPL)
summer$South <- with(summer, c[1]*BRPL)
summer$SouthW <- with(summer, c[2]*BRPL + MES)
summer$West <- with(summer, c[3]*BRPL + b[3]*NDPL)

summer_dist <- summer[, c(2,8:16)] 

write.csv(fall_dist, file = "fall_dis.csv")
write.csv(winter_dist, file = "winter_dis.csv")
write.csv(monsoon_dist, file = "monsoon_dis.csv")
write.csv(summer_dist, file = "summer_dis.csv")

