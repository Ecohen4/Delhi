library(ggplot2)
library(maptools)
library(rgeos)
library(RColorBrewer)
library(scales)
library(mapproj)
rm(list=ls())
setwd("~/Documents/Delhi/BTeam/Delhi")
dis <- readShapeSpatial ("Districts.shp")
dis <- fortify(dis, region = "OBJECTID")


# drops <- c("X","X.1", "X.2")
# map <- map[,!(names(map) %in% drops)]



data <- read.csv("Basic Info.csv")
colnames(data)[4] <- "pop11"

# colourCount = length(unique(data$pop11))
# getPalette = colorRampPalette(brewer.pal(9, "Blues"))

ggplot()+
  geom_map(data = data, aes(map_id = OBJECTID, fill= pop11, colour = DISTRICT),map=dis)+
  geom_path(data=dis, aes(x=long, y=lat, group=group), color = NA, size = 0.5, linemetre = 0.5)+
  coord_map(projection="mercator")+
  labs(title="Population Census Result in 2011", fill="") +
  theme(legend.position = "right")






