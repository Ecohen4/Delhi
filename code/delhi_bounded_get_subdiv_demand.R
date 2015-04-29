library(plyr)

setwd("~/Documents/github/Delhi/data")

# load in files
data1 <- read.csv("BRPL_feeder_cluster_readings_20110328_20120325.csv",header=TRUE)
data2 <- read.csv("BYPL_feeder_cluster_readings_20110328_20120325.csv",header=TRUE)
data3 <- read.csv("MES_feeder_cluster_readings_20110328_20120325.csv",header=TRUE)
data4 <- read.csv("NDPL_feeder_cluster_readings_20110328_20120325.csv",header=TRUE)
weight <- read.csv("subdiv_grid_weight.csv",header=TRUE)
mapping <- read.csv("grid_meter.csv",header=TRUE)

# reshape
data_comb <- rbind(data1[,c('mU','meter','datetime')],data2[,c('mU','meter','datetime')],
              data3[,c('mU','meter','datetime')],data4[,c('mU','meter','datetime')])
data <- data_comb[data_comb$meter %in% grep('sum', data_comb$meter, value=TRUE),]
data <- merge(data, mapping, by = c("meter")) # assign meters with grid names.
data <- merge(data, weight,by = c("grid_name")) # merge by grid name.
data$kw <- data$mU * as.numeric(sub("%","",data$usage_weight))/100

# compute demand for each subdivition in delhi
subdiv <- ddply(data, c("datetime", "subdiv_name"), summarise, demand = sum(kw)) # sum by subdiv and datetime

# split year, month, day, hour, minute
total <- subdiv
total$dt <- as.POSIXlt(total$datetime,format="%Y-%m-%d %H:%M:%S")
total$YR <- as.numeric(format(total$dt ,format = "%Y"))
total$M <- as.numeric(format(total$dt ,format = "%m"))
total$D <- as.numeric(format(total$dt ,format = "%d"))
total$HR <- as.numeric(format(total$dt ,format = "%H"))
total$MIN <- as.numeric(format(total$dt ,format = "%M"))

# write into csv file.
write.csv(total, file = "Delhi_subdiv_15min_demand.csv")


