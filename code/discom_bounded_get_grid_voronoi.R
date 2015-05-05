setwd("~/Documents/github/Delhi/data/Delhi .shp files/")
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
discom <- readOGR("delhi_licensee_final.shp", layer="delhi_licensee_final") # SpatialPolygonsDataFrame

# change projection to get planar coordinates
discom2 <- spTransform(discom, CRS("+init=epsg:32646"))
discomCol <- brewer.pal(5, "Pastel1")
plot(discom2, lwd=2, col=discomCol)

# Add labels for each discom
centers <- sapply(slot(discom2, "polygons"), slot, "labpt")
text(centers[1,], centers[2,], label=discom2 @ data$discom, cex=0.75, pos=1, col='blue')


#### grid points #####
# read grid point shape file
grid <- readOGR("grid_points.shp", layer="grid_points")
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
plot(gridArea, lty='dashed', add=TRUE)


###  bounded by discoms ######

# get a list of discom polygons
discom_list <- discom2@data$discom


data <- data.frame()
gridsOnly <- list()
points_list <- list()
plot(discom2)

# get grid area for each discom
for (i in 1:5) {
  
  # subset of discom shapefile : poly of the discom 
  discom_i <- discom2[discom2@data$discom == discom_list[i],]
  
  # subset spatialpoint : find points in the discom
  poly_coords <- discom_i@polygons[[1]]@Polygons[[1]]@coords
  points_coords <- grid2@coords
  
  # whether the points are in the polygon 
  inside <- point.in.polygon(points_coords[,1],points_coords[,2],poly_coords[,1],poly_coords[,2],mode.checked=T)
  points_i <- grid2[which(inside==1),]
  
  points_id <- points_i@data
  points_id$id_grid <- (1:nrow(points_id))
  points_id$id_discom <- discom_i@data$discom
  points_list <- rbind(points_id,points_list)
  
  
  # get gridarea within the discom
  gridarea_i <-voronoipolygons(points_i @ coords, discom_i)
  gridarea_i@data$grid <- points_i@data$grid
  gridarea_i@data$discom <- discom_i@data$discom
  
  union <- gUnaryUnion(discom_i, id=NULL)
  proj4string(union) <- proj4string(gridarea_i)
  gridsOnly[i] <- gIntersection(union, gridarea_i, byid=TRUE)
  
  plot(discom_i, lwd=2, col=discomCol[i], add=T)
  plot(gridsOnly[[i]],lty='dashed', add=TRUE)
  
  DridinDiscom <- data.frame(gridarea_i@data)
  DridinDiscom$polyArea <- gArea(gridsOnly[[i]], byid=TRUE) / 1e6  # area in km^2
  data <- rbind(data,DridinDiscom) 
}

points(grid2@coords, pch=19,cex=0.3)
text(grid2@coords, label=grid2 @ data$grid, cex=0.35, pos=1)

write.csv(data,"~/Documents/github/Delhi/data/discom_bounded_gridarea.csv")

### intersect with subdistricts #####

# create one spatialpolygons object for the grids
for (i in 1:length(gridsOnly)) {
 for (j in 1:length(gridsOnly[[i]])) {
   gridsOnly[[i]]@polygons[[j]]@ID <- paste(toString(i), toString(j), sep=" ")
 } 
}

shape <- gridsOnly[[1]]
for (i in 2:length(gridsOnly)) {
  shape <- spRbind(shape,gridsOnly[[i]])
}
proj4string(shape)  <- CRS("+init=epsg:32646")
#plot(shape)


# read in subdivition shapefile
subdiv <- readOGR("delhi_subdiv.shp", layer="delhi_subdiv") # SpatialPolygonsDataFrame
subdiv2 <- spTransform(subdiv, CRS("+init=epsg:32646")) # change projection to get planar coordinates
subdivCol <- brewer.pal(9, "Pastel1")
plot(subdiv2, lwd=2, col=subdivCol)


# intercet grids with subdivs
intesections <- gIntersection(subdiv2,shape, byid=TRUE)


# centroids of the 112 intersections
centroids <- coordinates(intesections)
x <- centroids[,1]
y <- centroids[,2]

ids <- sapply(slot(intesections,"polygons"), function(x) slot(x,'ID'))

plot(intesections)
points(centroids, pch=19,cex=0.3)
text(centroids,label=ids, cex=0.35, pos=1)


# create SpatialPolygonsDataFrame
intesections.df <- SpatialPolygonsDataFrame(intesections,data=data.frame(x=x, y=y, id=row.names(intesections)))

View(intesections.df@data)
intesections.df@data$area_intersection <- sapply(slot(intesections,"polygons"), function(x) slot(x,'area'))

colnames(intesections.df@data)
intesections.df@data$id <- sapply(intesections.df@data$id,function(x) toString(x))

id_refer <- sapply(intesections.df@data$id,function(x) unlist(strsplit(x, " ")))

intesections.df@data$id_subdiv <- as.numeric(id_refer[1,]) + 1
intesections.df@data$id_discom <- as.numeric(id_refer[2,])
intesections.df@data$id_grid <- as.numeric(id_refer[3,])
intesections.df@data$id_intersect <- (1:ncol(id_refer))


# calculate area
spdata <- intesections.df[,c(-3)]
View(spdata@data)

discom_list
index_discom <- unlist(lapply(spdata@data$id_discom,function(x) as.numeric(x)))
spdata@data$id_discom <- discom_list[index_discom]

subdiv2@data$subdiv
index_subdiv <- unlist(lapply(spdata@data$id_subdiv,function(x) as.numeric(x)))
spdata@data$id_subdiv <- subdiv2@data$subdiv[index_subdiv]

spdata@data <- merge(spdata@data,points_list,by=c("id_grid","id_discom"))

area_subdiv <- ddply(spdata@data,.(id_subdiv),summarize,area_subdiv=sum(area_intersection))
area_discom <- ddply(spdata@data,.(id_discom),summarize,area_discom=sum(area_intersection))

spdata@data <- merge(spdata@data,area_subdiv,by=c("id_subdiv"))
spdata@data <- merge(spdata@data,area_discom,by=c("id_discom"))

spdata@data$weight_in_subdiv <- spdata@data$area_intersection / spdata@data$area_subdiv
spdata@data$weight_in_discom <- spdata@data$area_intersection / spdata@data$area_discom

total <- spdata@data[,c(-3)]

#write weight to csv file
#write.csv(total,'~/Documents/github/Delhi/data/discom_bounded_intersection_weight.csv')
