# setwd("C:/git/core-transient")
#'#' Please download and install the following packages:
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)
library(tidyr)
library(ggplot2)
library(nlme)
library(gridExtra)
library(wesanderson)
library(stats)


# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_allscales = dplyr::rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")
sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

#temp
precip_temp = raster::getData("worldclim", var = "bio", res = 2.5) #supposed to be already /10 according to site
btest_temp = raster::extract(temp, sites[1:5,]) #just want bio1 -> mean annual temp 
#trying within latlon because 2001 test ran fine; keeping stateroute ID's associated
bbs_latlon$vartemp = raster::extract(temp, sites, buffer = 40000, fun = var)
#getData working for all but insanely slow to extract

#precip 
prec = raster::getData("worldclim", var = "bio", res = 2.5) #just want bio12 -> annual precip 
b_test_meanP = raster::extract(prec, sites[1:5,], buffer = 40000, fun = mean)
bbs_latlon$varP = raster::extract(prec, sites, buffer = 40000, fun = var)

#ndvi 
ndvi = raster(paste(ndvidata, "Vegetation_Indices_may-aug_2000-2010.gri", sep = "")) #can't find on getData
#coming out as a RasterLayer, converting to rasterstack 
ndvis = stack(ndvi)

ndvimean = ndvi/10000
btest_ndvi = raster::extract(ndvis, sites[1:5,], buffer = 40000, fun = mean)
bbs_latlon$varndvi = raster::extract(ndvimean, sites, buffer = 40000, fun = var)

#elev 
elev = raster::getData("worldclim", var = "alt", res = 2.5) #raster::getData("alt", country = 'USA', res = 2.5)
#coming out as a RasterLayer, converting to rasterstack 
elevs = stack(elev)

btest_elev = raster::extract(elevs, sites[1:5,], buffer = 40000, fun = mean)
bbs_latlon$varelev = raster::extract(elevs, sites, buffer = 40000, fun = var) 

bbs_envs = bbs_latlon 
#write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) 
#wrote file 03/28 w/elev from getData and using old env data; need to overwrite with updated env vars

#merge into bbs_allscales data