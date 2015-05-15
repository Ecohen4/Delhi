library(xlsx)
library(plyr)
library(reshape2)

setwd("~/Documents/github/Delhi")


####################
## prepare data ####
####################

setwd("/Users/yutian/Google Drive/global_trends/data/Demand_Data/Original/DELHI_08-13/big_data/11-12/data-01/week-01 Discom")
options(stringsAsFactors=FALSE)

### read in files ####
DISCOM <- read.csv("~/Documents/github/Delhi/bigfiles/Delhi_Discoms_Hourly_Demand.csv",header=T)

BYPL <- read.xlsx("BYPL.XLS",sheetName="Calculation", header=T)
BRPL <- read.xlsx("BRPL.XLS",sheetName="Calculation", header=T)
NDMC <- read.xlsx("NDMC.XLS",sheetName="Calculation", header=T)
NDPL <- read.xlsx("NDPL.XLS",sheetName="Calculation", header=T)
MES <- read.xlsx("MES.XLS",sheetName="Calculation", header=T)
ROHTAK <- read.xlsx("11KV+ ROHTAK ROAD.xls",sheetName="MF. & CLAC.", header=T)


### change into cleaner format ####
library(chron)
t1 <- chron("03/28/2011","00:00:00")
t2 <- chron("04/03/2011", "23:45:00")
deltat <- times("00:15:00")
tt <- seq(t1, t2, by = times("00:15:00"))
as.POSIXlt(tt[1])

### BRPL ###
BRPL_data <- BRPL[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699)-1,c(-1)]
colnames(BRPL_data) <- BRPL[1,-1]
head(BRPL_data)
str(BRPL_data)                
BRPL_data <- as.data.frame(sapply(BRPL_data, as.numeric))
BRPL_data <- data.frame(tt, BRPL_data)
colnames(BRPL_data)[1] <- "datetime"

BRPL_sum <- as.data.frame(rowSums(BRPL_data[,-1],na.rm=TRUE))
BRPL_sum <- data.frame(tt, BRPL_sum)
colnames(BRPL_sum) <- c("datetime","mU")


### BYPL ###
BYPL_data <- BYPL[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699)-1,c(-1)]
colnames(BYPL_data) <- BYPL[1,-1]
head(BYPL_data)
str(BYPL_data)                
BYPL_data <- as.data.frame(sapply(BYPL_data, as.numeric))
BYPL_data <- data.frame(tt, BYPL_data)
colnames(BYPL_data)[1] <- "datetime"

BYPL_sum <- as.data.frame(rowSums(BYPL_data[,-1],na.rm=TRUE))
BYPL_sum <- data.frame(tt, BYPL_sum)
colnames(BYPL_sum) <- c("datetime","mU")


### NDPL ###
NDPL_data <- NDPL[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699),c(-1)]
colnames(NDPL_data) <- NDPL[2,-1]
str(NDPL_data)                
NDPL_data <- as.data.frame(sapply(NDPL_data, as.numeric))
NDPL_data <- data.frame(tt, NDPL_data)
colnames(NDPL_data)[1] <- "datetime"

NDPL_sum <- as.data.frame(rowSums(NDPL_data[,-1],na.rm=TRUE))
NDPL_sum <- data.frame(tt, NDPL_sum)
colnames(NDPL_sum) <- c("datetime","mU")


### NDMC ###
NDMC_data <- NDMC[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699)-1,c(-1)]
colnames(NDMC_data) <- NDMC[1,-1]
str(NDMC_data)                
NDMC_data <- as.data.frame(sapply(NDMC_data, as.numeric))
NDMC_data <- data.frame(tt, NDMC_data)
colnames(NDMC_data)[1] <- "datetime"

NDMC_sum <- as.data.frame(rowSums(NDMC_data[,-1],na.rm=TRUE))
NDMC_sum <- data.frame(tt, NDMC_sum)
colnames(NDMC_sum) <- c("datetime","mU")



### MES ###
MES_data <- MES[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699)-1,c(-1)]
colnames(MES_data) <- MES[1,-1]
str(MES_data)                
MES_data <- as.data.frame(sapply(MES_data, as.numeric))
MES_data <- data.frame(tt, MES_data)
colnames(MES_data)[1] <- "datetime"

MES_sum <- as.data.frame(rowSums(MES_data[,-1],na.rm=TRUE))
MES_sum <- data.frame(tt, MES_sum)
colnames(MES_sum) <- c("datetime","mU")

### ROHTAK ###
ROHTAK2 <- ROHTAK[,c(29:70)]
ROHTAK_data <- ROHTAK2[c(4:99,104:199,204:299,304:399,404:499,504:599,604:699)-1,c(-1)]
colnames(ROHTAK_data) <- ROHTAK2[1,-1]
str(ROHTAK_data)                
ROHTAK_data <- as.data.frame(sapply(ROHTAK_data, as.numeric))
ROHTAK_data <- data.frame(tt, ROHTAK_data)
colnames(ROHTAK_data)[1] <- "datetime"


##################################
## test for eqaution accuracy ####
##################################

### BRPL ###
# for Less MES
list <- list("4865065","4865066","4865067","4865079","4865078","4865080","4865081")
map <- lapply(list, function(x) unlist(lapply(x,grep,colnames(MES_data))))
LessMES <- MES_data[,unlist(map)]
LessMES <- as.data.frame(rowSums(LessMES,na.rm=TRUE))
LessMES <- data.frame(tt, LessMES)
colnames(LessMES) <- c("datetime","mU")

# for Import from ROHTAK
list <- list("4902572","4864796","4864797","4864818","4864842","4864880","4864881")
map <- lapply(list, function(x) unlist(lapply(x,grep,colnames(ROHTAK_data))))
ROHTAK_BRPL <- ROHTAK_data[,unlist(map)]
ROHTAK_BRPL <- as.data.frame(rowSums(ROHTAK_BRPL,na.rm=TRUE))
ROHTAK_BRPL <- data.frame(tt, ROHTAK_BRPL)
colnames(ROHTAK_BRPL) <- c("datetime","mU")

BRPL_actual <- merge(BRPL_sum,LessMES,by="datetime")
BRPL_actual <- merge(BRPL_actual,ROHTAK_BRPL,by="datetime")
colnames(BRPL_actual) <- c("datetime","mU_sum","mU_MES","mU_ROHTAK")
BRPL_actual$mU <- BRPL_actual$mU_sum - BRPL_actual$mU_MES + BRPL_actual$mU_ROHTAK

BRPL_actual_hrly <- BRPL_actual[,c("datetime","mU")]

BRPL_actual_hrly$dt <- as.POSIXct(paste(as.Date(dates(BRPL_actual_hrly$datetime)),times(BRPL_actual_hrly$datetime)%%1))
BRPL_actual_hrly$YR  <- as.numeric(format(BRPL_actual_hrly$dt, "%Y"))
BRPL_actual_hrly$M  <- as.numeric(format(BRPL_actual_hrly$dt, "%m"))
BRPL_actual_hrly$D  <- as.numeric(format(BRPL_actual_hrly$dt, "%d"))
BRPL_actual_hrly$HR  <- as.numeric(format(BRPL_actual_hrly$dt, "%H"))
BRPL_actual_hrly$MIN  <- as.numeric(format(BRPL_actual_hrly$dt, "%M"))

BRPL_actual_hrly <- ddply(BRPL_actual_hrly[,c(-1,-3)],.(YR,M,D,HR),summarise, mU=sum(mU))



### BYPL ###
# for Import from ROHTAK
list <- list("4864794","4864795")
map <- lapply(list, function(x) unlist(lapply(x,grep,colnames(ROHTAK_data))))
ROHTAK_BYPL <- ROHTAK_data[,unlist(map)]
ROHTAK_BYPL <- as.data.frame(rowSums(ROHTAK_BYPL,na.rm=TRUE))
ROHTAK_BYPL <- data.frame(tt, ROHTAK_BYPL)
colnames(ROHTAK_BYPL) <- c("datetime","mU")

BYPL_actual <- merge(BYPL_sum,ROHTAK_BYPL,by="datetime")
colnames(BYPL_actual) <- c("datetime","mU_sum","mU_ROHTAK")
BYPL_actual$mU <- BYPL_actual$mU_sum + BYPL_actual$mU_ROHTAK

BYPL_actual_hrly <- BYPL_actual[,c("datetime","mU")]

BYPL_actual_hrly$dt <- as.POSIXct(paste(as.Date(dates(BYPL_actual_hrly$datetime)),times(BYPL_actual_hrly$datetime)%%1))
BYPL_actual_hrly$YR  <- as.numeric(format(BYPL_actual_hrly$dt, "%Y"))
BYPL_actual_hrly$M  <- as.numeric(format(BYPL_actual_hrly$dt, "%m"))
BYPL_actual_hrly$D  <- as.numeric(format(BYPL_actual_hrly$dt, "%d"))
BYPL_actual_hrly$HR  <- as.numeric(format(BYPL_actual_hrly$dt, "%H"))
BYPL_actual_hrly$MIN  <- as.numeric(format(BYPL_actual_hrly$dt, "%M"))

BYPL_actual_hrly <- ddply(BYPL_actual_hrly[,c(-1,-3)],.(YR,M,D,HR),summarise, mU=sum(mU))



### NDPL ####
# for Import from ROHTAK
list <- list("4864819","4864801","4864820","4865168","4864802","4864882","4902572","5128400")
map <- lapply(list, function(x) unlist(lapply(x,grep,colnames(ROHTAK_data))))
ROHTAK_NDPL <- ROHTAK_data[,unlist(map)]
ROHTAK_NDPL <- as.data.frame(rowSums(ROHTAK_NDPL,na.rm=TRUE))
ROHTAK_NDPL <- data.frame(tt, ROHTAK_NDPL)
colnames(ROHTAK_NDPL) <- c("datetime","mU")

NDPL_actual <- merge(NDPL_sum,ROHTAK_NDPL,by="datetime")
colnames(NDPL_actual) <- c("datetime","mU_sum","mU_ROHTAK")
NDPL_actual$mU <- NDPL_actual$mU_sum + NDPL_actual$mU_ROHTAK

NDPL_actual_hrly <- NDPL_actual[,c("datetime","mU")]

NDPL_actual_hrly$dt <- as.POSIXct(paste(as.Date(dates(NDPL_actual_hrly$datetime)),times(NDPL_actual_hrly$datetime)%%1))
NDPL_actual_hrly$YR  <- as.numeric(format(NDPL_actual_hrly$dt, "%Y"))
NDPL_actual_hrly$M  <- as.numeric(format(NDPL_actual_hrly$dt, "%m"))
NDPL_actual_hrly$D  <- as.numeric(format(NDPL_actual_hrly$dt, "%d"))
NDPL_actual_hrly$HR  <- as.numeric(format(NDPL_actual_hrly$dt, "%H"))
NDPL_actual_hrly$MIN  <- as.numeric(format(NDPL_actual_hrly$dt, "%M"))

NDPL_actual_hrly <- ddply(NDPL_actual_hrly[,c(-1,-3)],.(YR,M,D,HR),summarise, mU=sum(mU))


### NDMC ####
NDMC_actual_hrly <- NDMC_sum

NDMC_actual_hrly$dt <- as.POSIXct(paste(as.Date(dates(NDMC_actual_hrly$datetime)),times(NDMC_actual_hrly$datetime)%%1))
NDMC_actual_hrly$YR  <- as.numeric(format(NDMC_actual_hrly$dt, "%Y"))
NDMC_actual_hrly$M  <- as.numeric(format(NDMC_actual_hrly$dt, "%m"))
NDMC_actual_hrly$D  <- as.numeric(format(NDMC_actual_hrly$dt, "%d"))
NDMC_actual_hrly$HR  <- as.numeric(format(NDMC_actual_hrly$dt, "%H"))
NDMC_actual_hrly$MIN  <- as.numeric(format(NDMC_actual_hrly$dt, "%M"))

NDMC_actual_hrly <- ddply(NDMC_actual_hrly[,c(-1,-3)],.(YR,M,D,HR),summarise, mU=sum(mU))


### MES ###
MES_actual_hrly <- MES_sum

MES_actual_hrly$dt <- as.POSIXct(paste(as.Date(dates(MES_actual_hrly$datetime)),times(MES_actual_hrly$datetime)%%1))
MES_actual_hrly$YR  <- as.numeric(format(MES_actual_hrly$dt, "%Y"))
MES_actual_hrly$M  <- as.numeric(format(MES_actual_hrly$dt, "%m"))
MES_actual_hrly$D  <- as.numeric(format(MES_actual_hrly$dt, "%d"))
MES_actual_hrly$HR  <- as.numeric(format(MES_actual_hrly$dt, "%H"))
MES_actual_hrly$MIN  <- as.numeric(format(MES_actual_hrly$dt, "%M"))

MES_actual_hrly <- ddply(MES_actual_hrly[,c(-1,-3)],.(YR,M,D,HR),summarise, mU=sum(mU))


### Compare with actual demand : using equation ########
library(ggplot2)
BRPL_actual_hrly$city <- "Delhi - BRPL"
BYPL_actual_hrly$city <- "Delhi - BYPL"
NDPL_actual_hrly$city <- "Delhi - NDPL"
NDMC_actual_hrly$city <- "Delhi - NDMC"
MES_actual_hrly$city <- "Delhi - MES"

DISCOM_est1 <- rbind(BRPL_actual_hrly,BYPL_actual_hrly,NDPL_actual_hrly,
                     NDMC_actual_hrly,MES_actual_hrly)
  
compare_discom_1 <- merge(DISCOM,DISCOM_est1,by=c("city","YR","M","D","HR"))
compare_discom_1$MW <- compare_discom_1$MW / 1000
colnames(compare_discom_1)[6:7] <- c("mU_actual","mU_est") 
compare_discom_1$diff <- compare_discom_1$mU_actual - compare_discom_1$mU_est

compare_discom_1.1 <- melt(compare_discom_1, id=c("city","YR","M","D","HR"))
str(compare_discom_1.1)
compare_discom_1.1$variable <- as.character(compare_discom_1.1$variable)
compare_discom_1.1[which(compare_discom_1.1$variable=="mU_actual"),]$variable <- "actual"
compare_discom_1.1[which(compare_discom_1.1$variable=="mU_est"),]$variable <- "estimation_1"
compare_discom_1.1[which(compare_discom_1.1$variable=="diff"),]$variable <- "difference"
colnames(compare_discom_1.1)[6:7] <- c("item","mU")

compare_discom_1.1$dt <- strptime(paste(paste(compare_discom_1.1$M, compare_discom_1.1$D,compare_discom_1.1$YR,sep="/"),
                                        paste(compare_discom_1.1$HR, "00",sep=":")),format="%m/%d/%Y %H:%M")

str(compare_discom_1.1)

compare_discom_1.1 <- compare_discom_1.1[order(compare_discom_1.1$dt),]

ggplot(compare_discom_1.1,aes(x = dt,y = mU,color = item)) +
  labs(title="compare with estimation using equation") +
  geom_line() +
  facet_wrap (~city, scale="free_y",nrow=5)

## note they're completely the same 

### Compare with actual demand : using "company wise details" ########

grid_NDPL <- list(c("4864904", "4865062",	"4864905"),
                  c("4864912", "4864913"),
                  c("4864982", "4864983","4864953","4864984"),
                  c("4864939","4864940"),
                  c("4865034","4865035","4865036"),
                  c("4864886","4864887", "4864798", "4864799","4864888","5128402"),
                  c("4865057","4865058"),
                  c("4865054","4865055"),
                  c("4865056"))

grid_BRPL <- list(c("4864973","4864974","4864975","4864976"),
                  c("4864977","4865052"),
                  c("4864969","4864970","4864971","4864972"),
                  c("4864908","4864909"),
                  c("4864964" ,"4864965", "4864890", "4864891", "4864906","4864907"),
                  c("4864988","4864989"),
                  c("4864966","4864967","4864968"),
                  c("4864990","4864991"))

grid_BYPL <- list(c("4864910", "4864911"),
                  c("4864830"),
                  c("4864962", "4865033","4864902",	"4864903"),
                  c("4864979","4864980","4864981"),
                  c("4864992", "4864993" ,"4864914", "4865167","4864893","4864915"),
                  c("4864916","4864917"),
                  c("4865053","4864986"))
                          
grid_MES <- list(c("4864846","4864847","4864848","4864849","4864850"))
                  

  
names(grid_NDPL) <- c("GOPALPUR","SUBZI MANDI","ROHINI",	"SHALIMARBAGH", "NARELA", 
                 "NARAINA (NDPL)", "KASHMIRI GATE (DMRC)","KANJHAWALA","BAWANA")
names(grid_BRPL) <- c("PAPPANKALAN-1","PAPPANKALAN-2","NAJAFGARH","LODHI ROAD", 
                      "OKHLA","VASANT KUNJ", "MEHRAULI","SARITA VIHAR")
names(grid_BYPL) <- c("KASHMIRI GATE","NARAINA (BYPL)","PARK STREET","SOW", 
                      "PATPARGANJ"	,"GEETA COLONY"	,"GAZIPUR")
names(grid_MES) <- c("NARAINA MES")
       

grid <- list(grid_NDPL,grid_BRPL,grid_BYPL,grid_MES)
#########

# read NDPL
map <- lapply(grid[[1]], function(x) unlist(lapply(x,grep,colnames(NDPL_data))))
NDPL_grids_list <- lapply(map,function(x) as.data.frame(NDPL_data[,x]))
View(NDPL_grids_list[["NARAINA (NDPL)"]])
NDPL_grids <- lapply(names(NDPL_grids_list), function(x) rowSums(NDPL_grids_list[[x]],na.rm=T))
names(NDPL_grids) <- paste(names(NDPL_grids_list),"NDPL",sep="_")
NDPL_grids <- as.data.frame(NDPL_grids)
NDPL_grids <- cbind(tt,NDPL_grids)

# read BRPL
map <- lapply(grid[[2]], function(x) unlist(lapply(x,grep,colnames(BRPL_data))))
BRPL_grids_list <- lapply(map,function(x) as.data.frame(BRPL_data[,x]))
BRPL_grids <- lapply(names(BRPL_grids_list), function(x) rowSums(BRPL_grids_list[[x]],na.rm=T))
names(BRPL_grids) <- paste(names(BRPL_grids_list),"BRPL",sep="_")
BRPL_grids <- as.data.frame(BRPL_grids)
BRPL_grids <- cbind(tt,BRPL_grids)

# read BYPL
map <- lapply(grid[[3]], function(x) unlist(lapply(x,grep,colnames(BYPL_data))))
BYPL_grids_list <- lapply(map,function(x) as.data.frame(BYPL_data[,x]))
BYPL_grids <- lapply(names(BYPL_grids_list), function(x) rowSums(BYPL_grids_list[[x]],na.rm=T))
names(BYPL_grids) <- paste(names(BYPL_grids_list),"BYPL",sep="_")
BYPL_grids <- as.data.frame(BYPL_grids)
BYPL_grids <- cbind(tt,BYPL_grids)

# read MES
map <- lapply(grid[[4]], function(x) unlist(lapply(x,grep,colnames(MES_data))))
MES_grids_list <- lapply(map,function(x) as.data.frame(MES_data[,x]))
MES_grids <- lapply(names(MES_grids_list), function(x) rowSums(MES_grids_list[[x]],na.rm=T))
names(MES_grids) <- paste(names(MES_grids_list),"MES",sep="_")
MES_grids <- as.data.frame(MES_grids)
MES_grids <- cbind(tt,MES_grids)


### assign grid to discoms ####
Allgrids <- merge(NDPL_grids,BRPL_grids,by='tt')
Allgrids <- merge(Allgrids,BYPL_grids,by='tt')
Allgrids <- merge(Allgrids,MES_grids,by='tt')

discom_grid <- list(c("OKHLA","LODHI.ROAD","SARITA.VIHAR","VASANT.KUNJ","PAPPANKALAN.1","PAPPANKALAN.2","NAJAFGARH","MEHRAULI"),
                 c("SOW", "PATPARGANJ","GEETA.COLONY","GAZIPUR","KASHMIRI.GATE","NARAINA..BYPL._BYPL"),
                 c("NARAINA.MES"),
                 c("PARK.STREET"),
                 c("GOPALPUR" ,"SUBZI.MANDI","ROHINI","SHALIMARBAGH","NARELA","KANJHAWALA","BAWANA","NARAINA..NDPL."))
names(discom_grid) <- c("BRPL","BYPL","MES","NDMC","NDPL")  

DISCOM_data <- lapply(discom_grid, function(x) unlist(lapply(x,grep,colnames(Allgrids))))
DISCOM_data <- lapply(DISCOM_data, function(x) Allgrids[,x])

DISCOM_data_grids <- as.data.frame(DISCOM_data)

DISCOM_data_agg <- DISCOM_data
DISCOM_data_agg[[1]] <- rowSums(DISCOM_data_agg[[1]])
DISCOM_data_agg[[2]] <- rowSums(DISCOM_data_agg[[2]])
DISCOM_data_agg[[5]] <- rowSums(DISCOM_data_agg[[5]])

DISCOM_data_agg <- as.data.frame(DISCOM_data_agg)

DISCOM_data_agg <- cbind(tt,DISCOM_data_agg)
DISCOM_data_agg$tt <- as.POSIXct(paste(as.Date(dates(DISCOM_data_agg$tt)),times(DISCOM_data_agg$tt)%%1))
colnames(DISCOM_data_agg) <- c("tt","Delhi - BRPL","Delhi - BYPL","Delhi - MES","Delhi - NDMC","Delhi - NDPL")

DISCOM_data_agg <- melt(DISCOM_data_agg,id="tt")
colnames(DISCOM_data_agg) <- c("tt","city","mU")
DISCOM_data_agg$YR  <- as.numeric(format(DISCOM_data_agg$tt, "%Y"))
DISCOM_data_agg$M  <- as.numeric(format(DISCOM_data_agg$tt, "%m"))
DISCOM_data_agg$D  <- as.numeric(format(DISCOM_data_agg$tt, "%d"))
DISCOM_data_agg$HR  <- as.numeric(format(DISCOM_data_agg$tt, "%H"))
DISCOM_data_agg$MIN  <- as.numeric(format(DISCOM_data_agg$tt, "%M"))

DISCOM_est2 <- ddply(DISCOM_data_agg,.(city,YR,M,D,HR),summarise,mU=sum(mU))
DISCOM_est2$city <- as.character(DISCOM_est2$city)

compare_discom_2 <- merge(DISCOM, DISCOM_est2, by=c("city","YR","M","D","HR"))
compare_discom_2$MW <- compare_discom_2$MW / 1000
colnames(compare_discom_2)[6:7] <- c("mU_actual","mU_est") 
compare_discom_2$diff <- compare_discom_2$mU_actual - compare_discom_2$mU_est
compare_discom_2$diff_perc <- compare_discom_2$diff / compare_discom_2$mU_actual * 100

### compare value ####
compare_discom_2.1 <- melt(compare_discom_2[,-9], id=c("city","YR","M","D","HR"))
compare_discom_2.1$variable <- as.character(compare_discom_2.1$variable)
compare_discom_2.1[which(compare_discom_2.1$variable=="mU_actual"),]$variable <- "actual"
compare_discom_2.1[which(compare_discom_2.1$variable=="mU_est"),]$variable <- "estimation_2"
compare_discom_2.1[which(compare_discom_2.1$variable=="diff"),]$variable <- "difference"
colnames(compare_discom_2.1)[6:7] <- c("item","mU")

compare_discom_2.1$dt <- strptime(paste(paste(compare_discom_2.1$M, compare_discom_2.1$D,compare_discom_2.1$YR,sep="/"),
                                        paste(compare_discom_2.1$HR, "00",sep=":")),format="%m/%d/%Y %H:%M")
compare_discom_2.1 <- compare_discom_2.1[order(compare_discom_2.1$dt),]

ggplot(compare_discom_2.1,aes(x = dt,y = mU,color = item)) +
  labs(title="compare with estimation using 'company wise details'") +
  geom_line() +
  facet_wrap (~city, scale="free_y",nrow=5)

### plot percentage difference ####
compare_discom_2.2 <- melt(compare_discom_2[,c(-6,-7,-8)], id=c("city","YR","M","D","HR"))
compare_discom_2.2 <- compare_discom_2.2[,-6]
colnames(compare_discom_2.2)[6] <- "diff_perc"
compare_discom_2.2$dt <- strptime(paste(paste(compare_discom_2.2$M, compare_discom_2.2$D,compare_discom_2.2$YR,sep="/"),
                                        paste(compare_discom_2.2$HR, "00",sep=":")),format="%m/%d/%Y %H:%M")
compare_discom_2.2 <- compare_discom_2.2[order(compare_discom_2.2$dt),]

ggplot(compare_discom_2.2,aes(x = dt,y = diff_perc)) +
  labs(title="percentage difference using 'company wise details'") +
  geom_line() +
  facet_wrap (~city, scale="free_y",nrow=5)


