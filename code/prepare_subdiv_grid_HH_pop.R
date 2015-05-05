setwd("~/Documents/github/Delhi")

###################################
#### prepare subdiv level data ####
###################################

# read in population and partial hh information
hh_pop <- read.csv("data/consumption_household_population_data.csv",head=T)
hh_pop <- hh_pop[,c("District","Subdistrict2","num_of_households",
                    "population_persons","population_males",
                    "population_females","sex_ratio_2011",
                    "literacy_rate","percent_scheduled_caste_to_totalpop", 
                    "percent_nonworkers_to_total_pop")]
colnames(hh_pop) <- c("district","subdiv","num_of_households",
                      "num_of_population","num_of_males","num_of_females",
                      "sex_ratio", "percent_literacy",
                      "percent_scheduled_caste","percent_nonworkers")


# read in household information
hh_other <- read.csv("data/Delhi_subdiv_Housing_Census_2011.csv",head=T)
hh_other <- hh_other[,c( "district", "subdiv", "dilapidated_HH", "room_3plus",                                             
                         "assets_HH_with_TV_computer.laptop_TV_mobile_scooter.car")]
colnames(hh_other) <- c("district","subdiv","percent_dilapidated_hh", 
                        "percent_room_3more", "percent_tv.pc.ph.car")


# merge to get a complete data frame
hh_pop_all <- merge(hh_pop,hh_other,by=c("district", "subdiv"))
hh_pop_all$num_literacy <- hh_pop_all$percent_literacy * hh_pop_all$num_of_population / 100
hh_pop_all$num_scheduled_caste <- hh_pop_all$percent_scheduled_caste * hh_pop_all$num_of_population / 100
hh_pop_all$num_nonworkers <- hh_pop_all$percent_nonworkers * hh_pop_all$num_of_population / 100
hh_pop_all$num_dilapidated <- hh_pop_all$percent_dilapidated_hh * hh_pop_all$num_of_households / 100
hh_pop_all$num_room_3more <- hh_pop_all$percent_room_3more * hh_pop_all$num_of_households / 100
hh_pop_all$num_tv.pc.ph.car <- hh_pop_all$percent_tv.pc.ph.car * hh_pop_all$num_of_households / 100

#write.csv(hh_pop_all,"data/subdiv_hh_pop_7.csv")

################################################
#### Delhi bounded: prepare grid level data ####
################################################

#hh_pop_all <- read.csv("data/subdiv_hh_pop_7.csv",head=T)
library(plyr)

# calculate weight between grid, subdiv and intersection polygon between (grid,subdiv)
weight <- read.csv("data/delhi_bounded_intersection_weight.csv",header=TRUE)
#copy <- weight 
weight <- weight[,c("id_intersect","id_grid","id_subdiv","area_grid","weight_in_subdiv","weight_in_grid")]
colnames(weight)[1:3] <- c("intersect", "grid", "subdiv")

# make subdiv name consistent
sub_hp <- unique(hh_pop_all$subdiv)
sub_w <- unique(weight$subdiv)
sub_hp <- as.data.frame(sub_hp)
sub_w <- as.data.frame(sub_w)
sub_hp <- sub_hp[order(sub_hp),]
sub_w <- sub_w[order(sub_w),]
sub_hp <- as.data.frame(sub_hp)
sub_w <- as.data.frame(sub_w)

compare_subdivname <- cbind(sub_hp,sub_w)
colnames(compare_subdivname) <- c("sub_hp","subdiv")

weight <- merge(compare_subdivname,weight,by=c("subdiv"))
weight <- weight[,c(-1,-3)]
colnames(weight)[1] <- "subdiv"

# calculate for grid level pop hh data

# calculate at intersection level pop hh data
cal_grid_hh_pop <- merge(hh_pop_all,weight,by=c("subdiv"))
colnames(cal_grid_hh_pop)
cal_grid_hh_pop$num_of_households <- cal_grid_hh_pop$num_of_households * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_of_population <- cal_grid_hh_pop$num_of_population * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_of_males <- cal_grid_hh_pop$num_of_males * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_of_females <- cal_grid_hh_pop$num_of_females * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_literacy <- cal_grid_hh_pop$num_literacy * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_scheduled_caste <- cal_grid_hh_pop$num_scheduled_caste * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_nonworkers <- cal_grid_hh_pop$num_nonworkers * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_dilapidated <- cal_grid_hh_pop$num_dilapidated * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_room_3more <- cal_grid_hh_pop$num_room_3more * cal_grid_hh_pop$weight_in_subdiv
cal_grid_hh_pop$num_tv.pc.ph.car <- cal_grid_hh_pop$num_tv.pc.ph.car * cal_grid_hh_pop$weight_in_subdiv


# aggregate pop hh data to grid level
aggdata_grid <-aggregate(x = list( 
                        num_of_households = cal_grid_hh_pop$num_of_households,
                        num_of_population = cal_grid_hh_pop$num_of_population,
                        num_of_males = cal_grid_hh_pop$num_of_males,
                        num_of_females = cal_grid_hh_pop$num_of_females, 
                        num_literacy = cal_grid_hh_pop$num_literacy,
                        num_nonworkers = cal_grid_hh_pop$num_nonworkers,
                        num_scheduled_caste = cal_grid_hh_pop$num_scheduled_caste,
                        num_dilapidated = cal_grid_hh_pop$num_dilapidated, 
                        num_room_3more = cal_grid_hh_pop$num_room_3more,
                        num_tv.pc.ph.car  =cal_grid_hh_pop$num_tv.pc.ph.car 
                      ), 
                    by = list(cal_grid_hh_pop$grid), 
                    FUN = sum)

aggdata_grid$sex_ratio <- aggdata_grid$num_of_females / aggdata_grid$num_of_males * 1000
aggdata_grid$percent_literacy <- aggdata_grid$num_literacy / aggdata_grid$num_of_population * 100
aggdata_grid$percent_scheduled_caste <- aggdata_grid$num_scheduled_caste / aggdata_grid$num_of_population * 100
aggdata_grid$percent_nonworkers <- aggdata_grid$num_nonworkers / aggdata_grid$num_of_population * 100
aggdata_grid$percent_dilapidated <- aggdata_grid$num_dilapidated / aggdata_grid$num_of_households * 100
aggdata_grid$percent_room_3more <- aggdata_grid$num_room_3more / aggdata_grid$num_of_households * 100
aggdata_grid$percent_tv.pc.ph.car <- aggdata_grid$num_tv.pc.ph.car / aggdata_grid$num_of_households * 100

#write.csv(aggdata_grid,"data/delhi_bounded_grid_hh_pop_7.csv")

#################################################
#### Discom bounded: prepare grid level data ####
#################################################

