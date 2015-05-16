setwd("~/Google Drive/global_trends/data/Demand_Data/Original/DELHI_08-13/big_data/09-10/")

library(xlsx)
library(plyr)
library(reshape2)

options(stringsAsFactors=FALSE)

### read from raw data #####
filename <- list.files()
filepath <- filename[3:length(filename)]
filepath_BRPL <- paste(filepath, "/BRPL.XLS", sep="")
allBRPL <- lapply(filepath_BRPL, read.xlsx, sheetName="Calculation", header=FALSE)

### map to grid ####
names(allBRPL) <- filepath

# Count number of columns
colnum <- as.data.frame(lapply(allBRPL, ncol))

# grep() finding the columns corresponding to certain grid
strs <- c('BAWANA','GAZIPUR','GEETA COLONY','GOPALPUR', 'KANJHAWALA','KASHMIRI GATE','LODHI ROAD','MEHRAULI','NAJAFGARH','NARAINA','NARELA','OKHLA','PAPPANKALAN TX.','PAPPANKALAN-II','PARK STREET','PATPARGANJ','ROHINI','SARITA VIHAR','SHALIMARBAGH','SOW','SUBZI MANDI','VASANT KUNJ','IP STATION','RPH','GT')
stations <- allBRPL[[1]][2,1:85]
map <- lapply(strs, grep, x=as.character(stations), value=TRUE)
names(map) <- strs
map <- map[lapply(map,length)>0]
names_map <- names(map) 
station <- rbind(stations,colnames(stations))
colnames(station) <- as.character(station[1,])
station <- station[2,]

getcolname <- function (lst) {
  test <- as.character(station[,lst])
  return (test)
}

map <- lapply(map,getcolname)  # a map between grid and column names

### We will run the following loop to take raw data from the "allBRPL" list and 
### then clean each week's data and assign each week as an object in the "weeks" list
n <- length(allBRPL)
extract <- function (lis,data) {
  rows <- c(4:99, 104:199, 204:299, 304:399, 404:499, 504:599, 604:699)
  test <- data[rows,lis]
  return (test)  
}
# Create a new empty list.  The resulting "long" dataframes will be assigned to this list.
weeks <- list()

for(i in 1:n) {
  # Assign column names
  #colnames(allBRPL[[i]]) <- paste("X", 1:ncol(allBRPL[[i]]), sep="") # rename columns
  
  # Create weekDay variable
  weekDay <- allBRPL[[i]][813:819,1]
  weekDay_1 <- as.Date(weekDay, "%d/%m/%Y")
  weekDay_1 <- na.omit(weekDay_1)
  weekDay_2 <- as.numeric(weekDay)
  weekDay_2 <- na.omit(weekDay_2)
  weekDay_2 <- as.Date(weekDay_2, origin="1899/12/30")
  weekDay_2 <- as.Date(as.character(weekDay_2), format="%Y-%d-%m")
  weekDay <- c(weekDay_1,weekDay_2)
  weekDay <- sort(weekDay)
  days <- rep(weekDay, each=96)
  
  timeslot <- c("00:00", "00:30","01:00", "01:30","02:00", "02:30", "03:00", "03:30","04:00", "04:30",
                "05:00", "05:30","06:00", "06:30","07:00", "07:30", "08:00", "08:30","09:00", "09:30",
                "10:00", "10:30","11:00", "11:30","12:00", "12:30", "13:00", "13:30","14:00", "14:30",
                "15:00", "15:30","16:00", "16:30","07:00", "17:30", "18:00", "18:30","19:00", "19:30",
                "20:00", "20:30","21:00", "21:30","22:00", "22:30", "23:00", "23:30")
  timeslots <- rep(timeslot, 7*96) 

  data <- lapply(map,extract,data=allBRPL[[i]]) # list of each grid's observation (separate) in a week
  names(data) <- names_map
  data <- lapply(data, sapply, as.numeric)
  data <- lapply(data, as.data.frame)
  data_new <- lapply(data, rowSums)  

  # Reorganize data frame
  newBRPL <- data.frame(days, timeslots, data_new)
  #colnames(newBRP) <- c('date', 'time', strs)
  
  longBRPL <- melt(newBRPL, id=c("days", "timeslots"))
  colnames(longBRPL) <- c("date", "time", "grid", "mU")  # rename column names
  
  # Assign the resulting "long" form data to the "weeks" list
  weeks[[i]] <- longBRPL
}

names(weeks) <- filepath

formatted <- data.frame(weeks[[1]])
for (i in 2:length(weeks)){
  formatted <- rbind(formatted,weeks[[i]])
}

write.csv(formatted, file = "BRPL_grid_15min_demand_09-10.csv")

