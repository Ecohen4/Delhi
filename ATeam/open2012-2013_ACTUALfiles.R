### Objective: Open up all 50 'ACTUAL.xls' files and combine it into one dataset

options(stringsAsFactors=FALSE)

# Install and load "xlsx" package
# install.packages("xlsx")
require("xlsx")

# Product 2 digit character vectors representing week numbers
numbers1 <- paste("0", 1:9, sep="")  # Generate numbers 01, 02, ... 09
numbers2 <- as.character(10:50)      # Generate numbers 10 - 50
fifty <- c(numbers1, numbers2)       # Combines 01, 02, ... 09 with numbers 10 - 50

# Create vector of characters listing out paths to all 50 "ACTUAL.xls" files
paths <- paste("~/Documents/SLDC_big_data/12-13/data-", fifty, "/week-", fifty, "Discom/ACTUAL.xls", sep="")

# Create dataframes for each of the 50 weeks combined into 1 list
allData <- lapply(paths, read.xlsx, sheetName="Sheet1", header=FALSE)
objectNames <- paste("week", fifty, sep="")     # Name each object in list e.g. "week01" "week02" "week03" etc.
names(allData) <- objectNames

# Create a new empty list.  The resulting "long" dataframes will be assigned to this list.
weeks <- list()

### We will run the following loop to take raw data from the "allData" list and 
### then clean each week's data and assign each week as an object in the "weeks" list

n <- length(allData)

for(i in 1:n) {
# Assign column names
colnames(allData[[i]]) <- paste("X", 1:42, sep="") # rename columns

# Create weekDay variable
d1 <- allData[[i]][1,1]
d2 <- allData[[i]][1,8]
d3 <- allData[[i]][1,13]
d4 <- allData[[i]][1,18]
d5 <- allData[[i]][1,23]
d6 <- allData[[i]][1,28]
d7 <- allData[[i]][1,33]
weekDay <- c(d1,d2,d3,d4,d5,d6,d7)

days <- rep(weekDay, each=96)                       # This for the 'days' variable
timeslots <- rep(allData[[i]]$X1[4:99], 7)   # This is for the 'timeslots' variable

BRPL <- c(allData[[i]]$X2[4:99], allData[[i]]$X8[4:99], allData[[i]]$X13[4:99], allData[[i]]$X18[4:99], allData[[i]]$X23[4:99], allData[[i]]$X28[4:99], allData[[i]]$X33[4:99])
BYPL <- c(allData[[i]]$X3[4:99], allData[[i]]$X9[4:99], allData[[i]]$X14[4:99], allData[[i]]$X19[4:99], allData[[i]]$X24[4:99], allData[[i]]$X29[4:99], allData[[i]]$X34[4:99])
NDPL <- c(allData[[i]]$X4[4:99], allData[[i]]$X10[4:99], allData[[i]]$X15[4:99], allData[[i]]$X20[4:99], allData[[i]]$X25[4:99], allData[[i]]$X30[4:99], allData[[i]]$X35[4:99])
NDMC <-c(allData[[i]]$X5[4:99], allData[[i]]$X11[4:99], allData[[i]]$X16[4:99], allData[[i]]$X21[4:99], allData[[i]]$X26[4:99], allData[[i]]$X31[4:99], allData[[i]]$X36[4:99])
MES <- c(allData[[i]]$X6[4:99], allData[[i]]$X12[4:99], allData[[i]]$X17[4:99], allData[[i]]$X22[4:99], allData[[i]]$X27[4:99], allData[[i]]$X32[4:99], allData[[i]]$X37[4:99])

# Reorganize data frame
newData <- data.frame(days, timeslots, BRPL, BYPL, NDPL, NDMC, MES)

# Convert to numeric data
newData$BRPL <- as.numeric(newData$BRPL)
newData$BYPL <- as.numeric(newData$BYPL)
newData$NDPL <- as.numeric(newData$NDPL)
newData$NDMC <- as.numeric(newData$NDMC)
newData$MES <- as.numeric(newData$MES)

# Melt dataframe so that 'discom' is a variable
# install.packages("reshape")
library("reshape")

longData <- melt(newData, id=c("days", "timeslots"))
colnames(longData) <- c("days", "timeslots", "discom", "mW")  # rename column names
longData$mW <- as.numeric(longData$mW)
longData$timepoint <- rep(1:672, 5)

# Assign the resulting "long" form data to the "weeks" list
weeks[[i]] <- longData
}

# Create plots
require("ggplot2")

# Create vector of plot names
plotNames <- paste("plot", fifty, sep="")

# This loop assigns a plot of week 1 data to "plot01", week 2 data to "plot02" etc.
for(i in 1:n){
plot <- ggplot(weeks[[i]], aes(x=timepoint, y=mW, group=discom, col=discom)) + geom_line(size=1.1)
assign(plotNames[i], plot, envir=globalenv())
}

# Show grid of data by week
install.packages("gridExtra")
require("gridExtra")
grid.arrange(plot12, plot38)

# Create allWeeks dataset that binds all 50 week "long-form" datasets
allWeeks <- rbind(weeks[[1]], weeks[[2]], weeks[[3]], weeks[[4]], weeks[[5]],
                  weeks[[6]], weeks[[7]], weeks[[8]], weeks[[9]], weeks[[10]],
                  weeks[[11]], weeks[[12]], weeks[[13]], weeks[[14]], weeks[[15]],
                  weeks[[16]], weeks[[17]], weeks[[18]], weeks[[19]], weeks[[20]],
                  weeks[[21]], weeks[[22]], weeks[[23]], weeks[[24]], weeks[[25]],
                  weeks[[26]], weeks[[27]], weeks[[28]], weeks[[29]], weeks[[30]],
                  weeks[[31]], weeks[[32]], weeks[[33]], weeks[[34]], weeks[[35]],
                  weeks[[36]], weeks[[37]], weeks[[38]], weeks[[39]], weeks[[40]],
                  weeks[[41]], weeks[[42]], weeks[[43]], weeks[[44]], weeks[[45]],
                  weeks[[46]], weeks[[47]], weeks[[48]], weeks[[49]], weeks[[50]])

allWeeks$date <- as.Date(as.numeric(allWeeks$days), origin="1899-12-30")

# Export 3/26/2012 - 3/4/2013 data into .csv file
write.csv(allWeeks, "loads2012-2013.csv")

# Create dataframe containing peak load per day for each discom
dailyPeak <- aggregate(allWeeks$mW, by=list(allWeeks$date, allWeeks$discom), FUN = "max")
colnames(dailyPeak) <- c("date", "discom", "mW")  # rename column names

# Graph daily peak loads from 3/26/2012 - 3/4/2013
ggplot(dailyPeak, aes(x=date, y=mW, group=discom, col=discom)) + 
  geom_line(size=1.1) + 
  scale_x_date(breaks="1 month") + 
  ggtitle("Delhi - Daily Peak Loads from 3/26/2012 - 3/4/2013")

# Create separate plots for peak power loads of each utility
ggplot(dailyPeak, aes(x=date, y=mW, color=discom)) + 
  geom_line(size=1.1) + 
  facet_grid(discom ~ ., scales="free_y") +
  scale_x_date(breaks="1 month") + 
  ggtitle("Delhi - Daily Peak Loads from 3/26/2012 - 3/4/2013")

# Graph 15 minute interval loads from 3/26/2012 - 3/4/2013
ggplot(allWeeks, aes(x=date, y=mW, group=discom, col=discom)) + 
  geom_line(size=1.1) + 
  scale_x_date(breaks="1 month") + 
  ggtitle("Delhi Power Loads Measured Every 15 Minutes from 3/26/2012 - 3/4/2013")