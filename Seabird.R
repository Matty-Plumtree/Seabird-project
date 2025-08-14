library(devtools)
library(MRSea)
library(raster)
library(sf)
library(units)
library(ggforce)
library(dplyr)
library(data.table)
library(tidyr)
library(stars)
library(bayestestR)
library(Matrix)
library(stringr)
library(statmod)
library(dplyr)
library(scales)
library(MRSea)
library(lme4)
library(forcats)
library(broom)



#pick projection
UTM = 32630

file.depthgrid <- "//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Aerial Survey Data/MRSea/E3_envgrid.tif" ## raster for depth grid
file.birds <-"//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Aerial Survey Data/Survey Data/birds_24m.gpkg"

#load in shapefiles for each area
E3breed12<-st_transform(st_read("//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Boundary shapefile/E3coastal_jelmer.shp"),UTM)%>%
  dplyr::select()
E3array<-st_transform(st_read("//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Boundary shapefile/E3.shp"),UTM)%>%
  dplyr::select()
E3nonbreed12<-st_transform(st_read("//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Boundary shapefile/E3 12km buff.shp"),UTM)%>%
  dplyr::select()
#shapefile for windfarm
kinc<-st_transform(st_read("Shapefile/Kincardine.shp"),UTM)%>%
  dplyr::select()

FowlsH<-st_transform(st_read('Shapefile/FH shape.shp'),UTM)%>%
  dplyr::select()

CoastL<-st_transform(st_read('Shapefile/Coastline.shp'),UTM)%>%
  dplyr::select()

birds<-st_read(file.birds)

#gives the distances from the kinc shapefile to each bird
birdsf<-st_as_sf(birds,coords=c("Easting", "Northing"),crs=32630)
birdsf <- birdsf %>%
  mutate(distance = as.numeric(st_distance(geom, kinc)))
birdsf <- birdsf %>%
  mutate(distanceFowl = as.numeric(st_distance(geom, FowlsH)))

