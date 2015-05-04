
setwd("~/Documents/github/Delhi/data")

# read in coeffeicient and socioeconomic data
coefficient <- read.csv('coefficients.delhi.grids.csv', header=TRUE)
econ <- read.csv('consumption_household_population_data.csv',header=TRUE)
colnames(coefficient)[1] <- 'Subdistrict'

data <- merge(econ, coefficient, by = c('Subdistrict')) # a combined data frame.
data$cooling.per <- data$cooling/data$population_persons
data$heatng.per <- data$heating/data$population_persons

data$cooling.per[which(data$sig.cooling>0.1)] <- 0
data$heating.per[which(data$sig.heating>0.1)] <- 0
data$cooling.per[which(data$cooling.per<0)] <- 0
data$heating.per[which(data$heating.per>0)] <- 0
data$cooling.per <- data$cooling.per * 100000 # converting from MW to W/(degree C*Capita)
data$heating.per <- data$heating.per * 100000


#### 1. For "cooling demand intensity" ######

# 1.1 a glimpse about the predictors ####
attach(data)
par(mfrow=c(2,3))
par(oma = c(4, 1, 1, 1))

par(mfrow=c(2,3))
copy <- data
data <- data[which(data$cooling.per>0),]

plot(data$sex_ratio_2011, data$cooling.per, main='"Sex Ratio" vs "Cooling/Capita"', xlab="Sex Ratio", ylab="Watt/(°C*capita)", pch=19)
abline(fit <- lm(data$cooling.per~data$sex_ratio_2011), col="red")
legend("topright", bty="n", border = "black", legend=paste("p-value=", format(summary(fit)$coefficients[2,4] , digits=4)))

plot(data$Dilapidated, data$cooling.per, main='"Dilapidated Condition" vs "Cooling/Capita"', xlab="% of Households in Dilapidated Condition ", ylab="Watt/(°C*capita)", pch=19)
abline(fit <- lm(data$cooling.per~data$Dilapidated), col="red")
legend("topright", bty="n", border = "black", legend=paste("p-value=", format(summary(fit)$coefficients[2,4] , digits=4)))

plot(data$literacy_rate, data$cooling.per, main='"Literacy Rate" vs "Cooling/Capita"', xlab="Literacy Rate", ylab="Watt/(°C*capita)", pch=19)
abline(fit <- lm(data$cooling.per~data$literacy_rate), col="red")
legend("topright", bty="n", border = "black", legend=paste("p-value=", format(summary(fit)$coefficients[2,4] , digits=4)))

plot(data$percent_nonworkers_to_total_pop, data$cooling.per, main='"Nonworker Rate" vs "Cooling/Capita"', xlab="Nonworker Rate", ylab="Watt/(°C*capita)", pch=19)
abline(fit <- lm(data$cooling.per~data$percent_nonworkers_to_total_pop), col="red")
legend("topright", bty="n", border = "black", legend=paste("p-value=", format(summary(fit)$coefficients[2,4] , digits=4)))

plot(data$percent_scheduled_caste_to_totalpop, data$cooling.per, main='"Good Condition" vs "Cooling/Capita"', xlab="% of Households in Good Condition ", ylab="Watt/(°C*capita)", pch=19)
abline(fit <- lm(data$cooling.per~data$percent_scheduled_caste_to_totalpop), col="red")
legend("topright", bty="n", border = "black", legend=paste("p-value=", format(summary(fit)$coefficients[2,4] , digits=4)))

# note: missing "percent of households with TV, computer, phone and car" and "percent of households with 3 or more rooms"

# 1.2 regression ###
fit <- lm(data, cooling.per ~ sex_ratio_2011 + Dilapidated + literacy_rate + percent_nonworkers_to_total_pop + percent_scheduled_caste_to_totalpop)
summary(fit)

# 1.3 fixed effect on district ###
fixed_effect_data <- data
# dotchart
x <- fixed_effect_data[order(fixed_effect_data$cooling.per),] # sort by cooling.per
x$District <- factor(x$District) # it must be a factor
dotchart(x$cooling.per,labels=x$Subdistrict,cex=.7,main="Distribution of cooling demand intensity for 27 subdistricts in Delhi",xlab="Watt/(°C*capita)")

# Model fitting - models 

fit1<-lme(cooling.per ~ annual_percap_consump_KWh  + Good , random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit1)

fit2<-lme(cooling.per ~ annual_percap_consump_KWh  + Good + percent_nonworkers_to_total_pop, random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit2)

# add more predictors **
fit3 <- lme(cooling.per ~  annual_percap_consump_KWh + percent_nonworkers_to_total_pop + Good  +literacy_rate + growth_rate_2001_2011 + percent_child_to_totalpop +sex_ratio_2011, random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit3)

# remove "growth_rate_2001_2011" *** 
fit4 <- lme(cooling.per ~  annual_percap_consump_KWh + percent_nonworkers_to_total_pop + Good +literacy_rate + percent_child_to_totalpop + sex_ratio_2011, random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit4)

# remove "percent_child_to_totalpop" 
fit5 <- lme(cooling.per ~  annual_percap_consump_KWh + percent_nonworkers_to_total_pop + Good +literacy_rate + sex_ratio_2011 , random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit5)

# a better model  
fit6 <- lme(cooling.per ~  annual_percap_consump_KWh + I(percent_nonworkers_to_total_pop/percent_child_to_totalpop) + Good+literacy_rate + sex_ratio_2011 , random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit6)

# the one to compare with other methods
fit7 <- lme(cooling.per ~  sex_ratio_2011 + Dilapidated + literacy_rate + percent_nonworkers_to_total_pop + percent_scheduled_caste_to_totalpop, random = ~ 1 | District, data=fixed_effect_data, method="ML")
summary(fit7)


# show the covariance matrix 
library(gclus)
covar_data <- fixed_effect_data[,c('cooling.per','sex_ratio_2011','percent_nonworkers_to_total_pop', 'Dilapidated','literacy_rate','percent_scheduled_caste_to_totalpop')]

judge.cor <- cor(covar_data)
judge.color <- dmat.color(judge.cor,breaks=c(-1,-.85,0,.85,1), colors =cm.colors(5,alpha = .7))
judge.o <- order.single(judge.cor)
cpairs(covar_data[,c(-1)],judge.o,judge.color)


#### 2. For "heating demand intensity" ######



