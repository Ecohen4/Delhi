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

# test<-merge(data, dis, by.x="OBJECTID", by.y="id") # merge data frames (optional)

# colourCount = length(unique(data$pop11))
# getPalette = colorRampPalette(brewer.pal(9, "Blues"))

# categorical fill
ggplot()+
  geom_map(data = data, aes(map_id = OBJECTID, fill= DISTRICT, group = DISTRICT), map=dis) +
  geom_path(data=dis, aes(x=long, y=lat, group=group), color = NA, size = 0.5, linemetre = 0.5) + 
  scale_colour_brewer(type="seq", palette=length(levels(as.factor(data$DISTRICT))))

#characteristic fill
ggplot()+
  geom_map(data = data, aes(map_id = OBJECTID, fill= pop11), map=dis) +
  geom_path(data=dis, aes(x=long, y=lat, group=group), color = NA, size = 0.5, linemetre = 0.5) +
  coord_map(projection="mercator") +
  labs(title="Population Census Result in 2011") +
  theme(legend.position = "right")






