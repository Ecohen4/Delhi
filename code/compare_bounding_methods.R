setwd("~/Documents/github/Delhi")

#######################################
#### compare percentage difference ####
#######################################

delhi_bound <- read.csv("bigfiles/delhi_bounded_discom_demand_compare.csv")
discom_bound <- read.csv("bigfiles/discom_bounded_discom_demand_compare.csv")

compare_total <- delhi_bound[,c("discom","YR","M","D","HR")]
perc_delhi <- delhi_bound$diff_perc
perc_discom <- discom_bound$diff_perc

compare_total <- cbind(compare_total,perc_delhi,perc_discom)
compare_total$datetime <- as.POSIXlt(paste(compare_total$YR, compare_total$M, compare_total$D, compare_total$HR),
                       format = "%Y %m %d %H")

compare_BRPL <- compare_total[which(compare_total$discom=="BRPL"),]
compare_BYPL <- compare_total[which(compare_total$discom=="BYPL"),]
compare_MES <- compare_total[which(compare_total$discom=="MES"),]
compare_NDMC <- compare_total[which(compare_total$discom=="NDMC"),]
compare_NDPL <- compare_total[which(compare_total$discom=="NDPL"),]

compare_BRPL <- compare_BRPL[order(compare_BRPL$datetime),] 
compare_BYPL <- compare_BYPL[order(compare_BYPL$datetime),] 
compare_MES <- compare_MES[order(compare_MES$datetime),] 
compare_NDMC <- compare_NDMC[order(compare_NDMC$datetime),] 
compare_NDPL <- compare_NDPL[order(compare_NDPL$datetime),] 


# compare BRPL
plot(compare_BRPL$datetime,compare_BRPL$perc_delhi,type="l",col="red",
     main="Compare BRPL",xlab="time",ylab="difference(%)")
lines(compare_BRPL$datetime,compare_BRPL$perc_discom,col="green")
legend("topright", # places a legend at the appropriate place 
       c("Delhi_bound","discom_bound"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width

# compare BYPL
plot(compare_BYPL$datetime,compare_BYPL$perc_delhi,type="l",col="red",
     main="Compare BYPL",xlab="time",ylab="difference(%)")
lines(compare_BYPL$datetime,compare_BYPL$perc_discom,col="green")
legend("topright", # places a legend at the appropriate place 
       c("Delhi_bound","discom_bound"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width

# compare MES
plot(compare_MES$datetime,compare_MES$perc_delhi,type="l",col="red",
     main="Compare MES",xlab="time",ylab="difference(%)")
lines(compare_MES$datetime,compare_MES$perc_discom,col="green")
legend("topright", # places a legend at the appropriate place 
       c("Delhi_bound","discom_bound"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width

# compare NDMC
plot(compare_NDMC$datetime,compare_NDMC$perc_delhi,type="l",col="red",
     main="Compare NDMC",xlab="time",ylab="difference(%)")
lines(compare_NDMC$datetime,compare_NDMC$perc_discom,col="green")
legend("topright", # places a legend at the appropriate place 
       c("Delhi_bound","discom_bound"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width

# compare NDPL
plot(compare_NDPL$datetime,compare_NDPL$perc_delhi,type="l",col="red",
     main="Compare NDPL",xlab="time",ylab="difference(%)")
lines(compare_NDPL$datetime,compare_NDPL$perc_discom,col="green")
legend("topright", # places a legend at the appropriate place 
       c("Delhi_bound","discom_bound"), # puts text in the legend 
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","green")) # gives the legend lines the correct color and width


##################################
###### compare demand shape ######
##################################

compare_delhi<- read.csv("bigfiles/delhi_bounded_discom_demand_compare.csv",head=T)
compare_discom<- read.csv("bigfiles/discom_bounded_discom_demand_compare.csv",head=T)
colnames(compare_delhi)[8] <- "method1_MU"
colnames(compare_discom)[8] <- "method2_MU"
compare <- merge(compare_delhi,compare_discom,by= c("discom","YR","M","D","HR","MU_1"))
colnames(compare)
compare <- compare[,c(-7,-9,-10,-11,-13,-14)]
colnames(compare)[6] <- "original_MU"
compare$dt <- as.POSIXct(paste(compare$YR, compare$M,compare$D,compare$HR),format="%Y%m%d %H")

BRPL  <- compare[which(compare$discom=="BRPL"),]
BYPL <- compare[which(compare$discom=="BYPL"),]
MES <- compare[which(compare$discom=="MES"),]
NDMC <- compare[which(compare$discom=="NDMC"),]
NDPL<- compare[which(compare$discom=="NDPL"),]

BRPL <- BRPL[order(BRPL$dt),]
plot(BRPL$dt,BRPL$original_MU,"l",ylim=c(0, 3),col="green",main = "compare BRPL")
lines(BRPL$dt,BRPL$method1_MU,col="red")
lines(BRPL$dt,BRPL$method2_MU,col="blue")
legend("topright",
       c("original","method1","method2"),
       col=c( 'green','red', 'blue'),
       lty=1, bty='n', cex=.75)

BYPL <- BYPL[order(BYPL$dt),]
plot(BYPL$dt,BYPL$original_MU,"l",ylim=c(0, 1.5),col="green",main = "compare BYPL")
lines(BYPL$dt,BYPL$method1_MU,col="red")
lines(BYPL$dt,BYPL$method2_MU,col="blue")
legend("topright",
       c("original","method1","method2"),
       col=c( 'green','red', 'blue'),
       lty=1, bty='n', cex=.75)

MES <- MES[order(MES$dt),]
plot(MES$dt,MES$original_MU,"l",ylim=c(0, 0.25),col="green",main = "compare MES")
lines(MES$dt,MES$method1_MU,col="red")
lines(MES$dt,MES$method2_MU,col="blue")
legend("topright",
       c("original","method1","method2"),
       col=c( 'green','red', 'blue'),
       lty=1, bty='n', cex=.75)

NDMC <- NDMC[order(NDMC$dt),]
plot(NDMC$dt,NDMC$original_MU,"l",ylim=c(0, 0.45),col="green",main = "compare NDMC")
lines(NDMC$dt,NDMC$method1_MU,col="red")
lines(NDMC$dt,NDMC$method2_MU,col="blue")
legend("topright",
       c("original","method1","method2"),
       col=c( 'green','red', 'blue'),
       lty=1, bty='n', cex=.75)

NDPL <- NDPL[order(NDPL$dt),]
plot(NDPL$dt,NDPL$original_MU,"l",ylim=c(0, 1.5),col="green",main = "compare NDPL")
lines(NDPL$dt,NDPL$method1_MU,col="red")
lines(NDPL$dt,NDPL$method2_MU,col="blue")
legend("topright",
       c("original","method1","method2"),
       col=c( 'green','red', 'blue'),
       lty=1, bty='n', cex=.75)

# ggplot #####
library(reshape2)
library(ggplot2)

compare2 <- melt(compare, id=c("discom","dt","YR","M","D","HR"))
str(compare2)

compare2$variable <- as.character(compare2$variable)
compare2[which(compare2$variable=="original_MU"),]$variable <- "original"
compare2[which(compare2$variable=="method1_MU"),]$variable <- "delhi_bound"
compare2[which(compare2$variable=="method2_MU"),]$variable <- "discom_bound"

compare2$value <- compare2$value * 1000
colnames(compare2)[7:8] <- c("method","MW")
compare2$method <- as.factor(compare2$method)

compare2 <- compare2[order(compare2$dt),]

# hourly resolution
ggplot(compare2,aes(x = dt,y = MW,color = method)) +
         geom_line() +
         facet_wrap (~discom, scale="free_y",nrow=5)

# daily resolution
compare3 <- aggregate(x = list(MW = compare2$MW), 
                      by = list(compare2$discom,
                                compare2$YR,
                                compare2$M,
                                compare2$D,
                                compare2$method), 
                      FUN = sum)
colnames(compare3) <- c("discom","YR","M","D","method","MW")
compare3$dt <- as.POSIXct(paste(compare3$YR, compare3$M,compare3$D),format="%Y%m%d")
compare3 <- compare3[order(compare3$dt),]

ggplot(compare3,aes(x = dt,y = MW/1000,color = method)) +
  geom_line() +
  facet_wrap (~discom, scale="free_y", nrow=5)

# daily resolution
compare4 <- compare3
compare4$day <- weekdays(as.Date(compare4$dt))

compare3 <- aggregate(x = list(MW = compare2$MW), 
                      by = list(compare2$discom,
                                compare2$YR,
                                compare2$M,
                                compare2$D,
                                compare2$method), 
                      FUN = sum)
colnames(compare3) <- c("discom","YR","M","D","method","MW")
compare3$dt <- as.POSIXct(paste(compare3$YR, compare3$M,compare3$D),format="%Y%m%d")
compare3 <- compare3[order(compare3$dt),]

ggplot(compare3,aes(x = dt,y = MW,color = method)) +
  geom_line() +
  facet_wrap (~discom, nrow=5)

