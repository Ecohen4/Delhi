library(xlsx)
library(plyr)
library(reshape2)

setwd("~/Documents/github/Delhi")

# read in old discom bounded grid-discom mapping
grid_discom_old <- read.csv("data/discom_bounded_intersection_weight.csv",head=T)

################################
## test for eqaution accuracy ##
################################
setwd("/Users/yutian/Google Drive/global_trends/data/Demand_Data/Original/DELHI_08-13/big_data/11-12/data-01/week-01 Discom")
options(stringsAsFactors=FALSE)

filename <- list.files()


### BRPL ###
files <- c("BRPL.XLS","MES.XLS","11KV+ ROHTAK ROAD.xls")
BRPL <- read.xlsx("BRPL.XLS",sheetName="Calculation", header=T)
MES <- read.xlsx("BRPL.XLS",sheetName="Calculation", header=T)

df3 <-

### read in MES ###
filepath_MES <- lapply(filepath, function(x) grep("MES.XLS",list.files(x),value=TRUE))
filepath_MES <- paste(filepath,filepath_MES,sep="/")
filepath_MES <- filepath_MES[-(45:51)]

allMES <- lapply(filepath_MES, read.xlsx, sheetName="Calculation", header=FALSE)


### map to grid ####
names(allMES) <- filepath1[-(45:51)]

# Count number of columns
colnum <- as.data.frame(lapply(allMES, ncol))

# grep() finding the columns corresponding to certain grid
list<- list(c("4864848","4864849","4864846","4864847","4864850"),
            c("4864804", "4865163"),
            c("4865065","4865066","4865067","4865078","4865079","4865080","4865081"))
names(list) <- c('NARAINA','RIDGE VALLE','others')

map <- lapply(list, function(x) unlist(lapply(x,grep,allMES[[1]][2,])))
map <- lapply(map,function(x) colnames(allMES[[1]])[x])


### We will run the following loop to take raw data from the "allBRPL" list and
### then clean each week's data and assign each week as an object in the "weeks" list
n <- length(allMES)
# Create a new empty list.  The resulting "long" dataframes will be assigned to this list.
weeks <- list()

for(i in 1:n) {
  # Assign column names
  #colnames(allBRPL[[i]]) <- paste("X", 1:ncol(allBRPL[[i]]), sep="") # rename columns

  # Create weekDay variable
  weekDay <- allMES[[i]][c(2,102,202,302,402,502,602),1]
  weekDay <- sort(weekDay)
  weekDay2 <- as.numeric(weekDay)
  weekDay2 <- as.Date(weekDay_2, origin="1899/12/30")
  days <- rep(weekDay2, each=96)

  timeslot <- c("00:00", "00:30","01:00", "01:30","02:00", "02:30", "03:00", "03:30","04:00", "04:30",
                "05:00", "05:30","06:00", "06:30","07:00", "07:30", "08:00", "08:30","09:00", "09:30",
                "10:00", "10:30","11:00", "11:30","12:00", "12:30", "13:00", "13:30","14:00", "14:30",
                "15:00", "15:30","16:00", "16:30","07:00", "17:30", "18:00", "18:30","19:00", "19:30",
                "20:00", "20:30","21:00", "21:30","22:00", "22:30", "23:00", "23:30")
  timeslots <- rep(timeslot, 7*96)

  data <- lapply(map,function(x) allMES[[i]][c(4:99,104:199,204:299,304:399,404:499,504:599,604:699),x]) # list of each feeder's observation (separate) in a week
  View(data[1])

  data <- lapply(data, sapply, as.numeric)
  data_new <- lapply(data, rowSums)

  # Reorganize data frame
  newMES <- data.frame(days, timeslots, data_new)
  #colnames(newBRP) <- c('date', 'time', strs)

  longMES <- melt(newMES, id=c("days", "timeslots"))
  colnames(longMES) <- c("date", "time", "feeder", "mU")  # rename column names

  # Assign the resulting "long" form data to the "weeks" list
  weeks[[i]] <- longMES
}

names(weeks) <- names(allMES)
View(weeks[1])

formatted <- data.frame(weeks[[1]])

for (i in 2:length(weeks)){
  formatted <- rbind(formatted,weeks[[i]])
}

write.csv(formatted, file = "~/Documents/github/Delhi/bigfiles/MES_feeder_15min_demand_11-12.csv")

### compare with another source##
formatted <- read.csv("bigfiles/MES_feeder_15min_demand_11-12.csv",head=T)
data_discom <- read.csv('bigfiles/Delhi_Girds_Hourly_Demand.csv',head=T)
data_MES <- formatted
data_MES$dt <- as.POSIXlt(paste(data_MES$date,data_MES$time))
data_MES$YR <- as.numeric(format(data_MES$dt ,format = "%Y"))
data_MES$M <- as.numeric(format(data_MES$dt ,format = "%m"))
data_MES$D <- as.numeric(format(data_MES$dt ,format = "%d"))
data_MES$HR <- as.numeric(format(data_MES$dt ,format = "%H"))
data_MES$MIN <- as.numeric(format(data_MES$dt ,format = "%M"))


NARAINA <- data_MES[which(data_MES$feeder=="NARAINA"),]
RIDGE.VALLE <- data_MES[which(data_MES$feeder=="RIDGE.VALLE"),]
others <- data_MES[which(data_MES$feeder=="others"),]

newdata <- NARAINA[order(NARAINA$dt),]
head(newdata)

NARAINA <- NARAINA[which(order(NARAINA$dt)),]
RIDGE.VALLE <- RIDGE.VALLE[order(RIDGE.VALLE$dt),]
others <- others[order(others$dt),]

head(NARAINA)
head(RIDGE.VALLE)
head(others)

data_MES2 <- cbind(NARAINA,RIDGE.VALLE$mU,others$mU)
data_MES2 <- data_MES2[,c(-1,-4)]
head(data_MES2)

colnames(data_MES2)[c(5,12:13)] <- c("NARAINA_mU","RIDGE.VALLE_mU","others_mU"  )

data_MES_hrly <- ddply(data_MES,.(date,HR),summarize,area_discom=sum(area_intersection))



