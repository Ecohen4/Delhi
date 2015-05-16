library(plyr)
setwd("~/Documents/github/Delhi/")

##########################################################
##### Compare with discom demand from another source #####
##########################################################

#### Get hourly demand for grids ####

# read in grid level demand data ####
demand_grid <- read.csv('bigfiles/allGrids_20110328_20120325.csv',header=T)
demand_grid <- demand_grid[,c(-1,-5)]
 
# split "datetime" into multiple columns ####
demand_grid$dt <- as.POSIXlt(demand_grid$datetime,format="%Y-%m-%d %H:%M:%S")
demand_grid$dt2 <-  as.POSIXlt(format(demand_grid$dt ,format = "%Y-%m-%d"))

demand_grid$YR <- as.numeric(format(demand_grid$dt ,format = "%Y"))
demand_grid$M <- as.numeric(format(demand_grid$dt ,format = "%m"))
demand_grid$D <- as.numeric(format(demand_grid$dt ,format = "%d"))
demand_grid$HR <- as.numeric(format(demand_grid$dt ,format = "%H"))
demand_grid$MIN <- as.numeric(format(demand_grid$dt ,format = "%M"))

demand_grid <- demand_grid[,c(-2)]

# summing over hours for each grid ####
tmp1 <- demand_grid$dt
tmp2 <- demand_grid$dt2

demand_grid$dt2 <- as.character(demand_grid$dt2)
demand_grid$dt <- as.character(demand_grid$dt)

hrly_demand <- ddply(demand_grid, c("meter","dt2","HR"), summarise, hrly_MU = sum(MU) )
demand_grid <- merge(hrly_demand,demand_grid,by=c("meter","dt2","HR"))
  
demand_grid$dt <- tmp1
demand_grid$dt2 <- tmp2

rm(tmp1)
rm(tmp2)


# get formatted hourly demand for the grids ####
colnames(hrly_demand) <- c("grid","dt","HR","MU")
str(hrly_demand)
hrly_demand$dt <- as.POSIXlt(hrly_demand$dt,format="%Y-%m-%d")
hrly_demand$YR <- as.numeric(format(hrly_demand$dt ,format = "%Y"))
hrly_demand$M <- as.numeric(format(hrly_demand$dt ,format = "%m"))
hrly_demand$D <- as.numeric(format(hrly_demand$dt ,format = "%d"))
hrly_demand$MIN <- 0

hrly_demand <- hrly_demand[,c(-2)]

# write to csv file
write.csv(hrly_demand,"bigfiles/Delhi_Girds_Hourly_Demand.csv")

#### Estimate hourly demand for discoms from grids ####

# 1.read in discom demand from another source ####
discom_demand1 <- read.csv("bigfiles/Delhi_Discoms_Hourly_Demand.csv",head=T)

# change into a comparable format
discom_demand1$MU <- discom_demand1$MW / 1000 
discom_demand1 <- discom_demand1[,c(-6)]
id_discom <- sapply(discom_demand1$city,function(x) strsplit(as.character(x)," "))
test <- as.data.frame(id_discom)
discom_demand1$id_discom <- t(test)[,3]
rm(test)
discom_demand1 <- discom_demand1[,c(-1)]

# 2.estimate discom demand #####

# read in some files
mapping <- read.csv("data/discom_bounded_intersection_weight.csv")
mapping <- mapping[,c("id_discom","grid")]
mapping <- unique(mapping)

grid_meter <- read.csv("data/grid_meter.csv")
colnames(grid_meter) <- c("grid","meter")

# add "discom" column to the hourly demand data
refer <- merge(grid_meter,mapping, by=c('grid'))
hrly_demand2 <-hrly_demand
colnames(hrly_demand2)[2] <- "meter"
hrly_demand2 <- merge(hrly_demand2,refer, by=c("meter"))
hrly_demand2 <- hrly_demand2[,c(-1)]

# summing over same discoms
discom_demand2 <- ddply(hrly_demand2, c("id_discom","YR","M","D","HR"),summarise,discom_MU = sum(MU))

# 3. compare accross versions #####
test <- merge(discom_demand1,discom_demand2,by=c("id_discom","YR","M","D","HR"))
colnames(test) <- c("discom","YR","M","D","HR","MU_1","MU_2")

test$diff <- test$MU_1 - test$MU_2
test$diff_perc <- test$diff / test$MU_1

plot((1:length(test$diff)),test$diff,'l',col = "blue")
plot((1:length(test$diff_perc)),test$diff_perc,'l',col = "red")

write.csv(test,"bigfiles/discom_bounded_discom_demand_compare.csv")

