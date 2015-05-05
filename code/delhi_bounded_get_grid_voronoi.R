setwd("~/Documents/github/Delhi/")
# Load packages
require('rgdal')
require('sp')
require('rgeos')
require('RColorBrewer')
require('deldir')
require('maptools')
require('plyr')

#### discom polygons #####
# read discom shape file
discom <- readOGR("data/Delhi .shp files/delhi_licensee_final.shp", layer="delhi_licensee_final") # SpatialPolygonsDataFrame

# change projection to get planar coordinates
discom2 <- spTransform(discom, CRS("+init=epsg:32646"))
discomCol <- brewer.pal(5, "Pastel1")
plot(discom2, lwd=2, col=discomCol)

# Add labels for each discom
centers <- sapply(slot(discom2, "polygons"), slot, "labpt")
text(centers[1,], centers[2,], label=discom2 @ data$discom, cex=0.75, pos=1, col='blue')


#### grid points #####
# read grid point shape file
grid <- readOGR("data/Delhi .shp files/grid_points.shp", layer="grid_points")
grid2 <- spTransform(grid, CRS("+init=epsg:32646"))
plot(grid2,add=T)#  plot the two shape files together.



### define voronoipolygons function ####
voronoipolygons <- function(x,poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bbox(poly)))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],y=crds[,2], row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))))
  
  return(voronoi)  
}


### bounded by Delhi #####
gridArea <-voronoipolygons(grid2 @ coords, discom2)
#plot(gridArea, lty='dashed', add=TRUE)

gridArea <-voronoipolygons(grid2 @ coords, discom2)
gridArea@data$grid <- discom2@data$grid
gridArea@data$discom <- discom2@data$discom2

union <- gUnaryUnion(discom2, id=NULL)
proj4string(union) <- proj4string(gridArea)
gridsOnly <- gIntersection(union, gridArea, byid=TRUE)
proj4string(gridsOnly)  <- CRS("+init=epsg:32646")

plot(discom2, lwd=2, col=discomCol)
plot(gridsOnly,lty='dashed', add=T)

plot(gridsOnly)


### intersect with subdistricts #####
# read in subdivition shapefile
subdiv <- readOGR("data/Delhi .shp files/delhi_subdiv.shp", layer="delhi_subdiv") # SpatialPolygonsDataFrame
subdiv2 <- spTransform(subdiv, CRS("+init=epsg:32646")) # change projection to get planar coordinates
subdivCol <- brewer.pal(9, "Pastel1")
plot(subdiv2, lwd=2, col=subdivCol)


# intercet grids with subdivs
intesections <- gIntersection(subdiv2,gridsOnly, byid=TRUE)

# intercet intesections with discoms
intesections2 <- gIntersection(discom2,intesections, byid=TRUE)


# centroids of the 158 intersections
centroids <- coordinates(intesections2)
x <- centroids[,1]
y <- centroids[,2]

ids <- sapply(slot(intesections2,"polygons"), function(x) slot(x,'ID'))

plot(intesections2)
points(centroids, pch=19,cex=0.3)
text(centroids,label=ids, cex=0.35, pos=1)


# create SpatialPolygonsDataFrame
intesections.df <- SpatialPolygonsDataFrame(intesections2,data=data.frame(x=x, y=y, id=row.names(intesections2)))

View(intesections.df@data)
intesections.df@data$area_intersection <- sapply(slot(intesections2,"polygons"), function(x) slot(x,'area'))

colnames(intesections.df@data)
intesections.df@data$id <- sapply(intesections.df@data$id,function(x) toString(x))

id_refer <- sapply(intesections.df@data$id,function(x) unlist(strsplit(x, " ")))

intesections.df@data$id_discom <- as.numeric(id_refer[1,]) +1
intesections.df@data$id_subdiv <- as.numeric(id_refer[2,]) + 1
intesections.df@data$id_grid <- as.numeric(id_refer[4,])
intesections.df@data$id_intersect <- (1:ncol(id_refer))


# calculate area
spdata <- intesections.df[,c(-3)]
View(spdata@data)

discom_list <- discom2$discom
index_discom <- unlist(lapply(spdata@data$id_discom,function(x) as.numeric(x)))
spdata@data$id_discom <- discom_list[index_discom]

subdiv2@data$subdiv
index_subdiv <- unlist(lapply(spdata@data$id_subdiv,function(x) as.numeric(x)))
spdata@data$id_subdiv <- subdiv2@data$subdiv[index_subdiv]

grid2@data$grid
index_grid <- unlist(lapply(spdata@data$id_grid,function(x) as.numeric(x)))
spdata@data$id_grid <- grid2@data$grid[index_grid]

area_subdiv <- ddply(spdata@data,.(id_subdiv),summarize,area_subdiv=sum(area_intersection))
area_discom <- ddply(spdata@data,.(id_discom),summarize,area_discom=sum(area_intersection))
area_grid <- ddply(spdata@data,.(id_grid),summarize,area_grid=sum(area_intersection))

spdata@data <- merge(spdata@data,area_subdiv,by=c("id_subdiv"))
spdata@data <- merge(spdata@data,area_discom,by=c("id_discom"))
spdata@data <- merge(spdata@data,area_grid,by=c("id_grid"))

spdata@data$weight_in_subdiv <- spdata@data$area_intersection / spdata@data$area_subdiv
spdata@data$weight_in_discom <- spdata@data$area_intersection / spdata@data$area_discom
spdata@data$weight_in_grid <- spdata@data$area_intersection / spdata@data$area_grid

#write weight to csv file
write.csv(spdata@data,'~/Documents/github/Delhi/data/delhi_bounded_intersection_weight.csv')
