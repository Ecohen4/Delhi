setwd("/Users/yutian/Documents/github/Delhi/data/")

area_df <- read.csv("area_ratio_discom_subdiv.csv",header=TRUE)
data <- read.csv('Delhi_coefficient_socioecon.csv',header=TRUE)

weight <- area_df[,c("intersection_id" , "subdiv_ratio" ,"subdiv_name","discom_name")]
colnames(weight) <- c("intersection_id", "subdiv_ratio" ,"Subdistrict","discom_name")

population <- merge(data[,c("Subdistrict","District","population_persons")],weight,by=c('Subdistrict'))
population$intersection_pop <- population[,"population_persons"]* population[,"subdiv_ratio"]
discom_pop <- aggregate(.~discom_name,FUN=sum, population[,c("intersection_pop","discom_name")])
colnames(discom_pop) <- c("discom","population")

discom_pop$population <- as.integer(discom_pop$population )
write.csv(discom_pop,"Delhi_discom_population.csv")

