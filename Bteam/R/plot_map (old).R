# MOVE TO D3, NO LONGER IN USE.......

# library(ggplot2)
# library(maptools)
# library(rgeos)
# library(scales)
# library(RColorBrewer)
# 
# setwd("~/Documents/Delhi/BTeam/Delhi")
# 
# district <- readShapeSpatial("Districts.shp")
# district <- fortify(district, region = "DISTRICT")
# 
# #needs to input data from total_daily.Rdata to color the map with daily energy consumption
# 
# # ggplot() +
# #   geom_map(data=data, aes(map_id=geoid, fill=percent), map=district) +
# #   geom_path(data=district, aes(x=long, y=lat, group=group), colour="black", size=0.25)+
# #   coord_map(projection = "mercator")
