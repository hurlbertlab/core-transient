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
geog = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography/"

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_allscales = dplyr::rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")
sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
#points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

#find proj of elev -> may need to do for each raster set? 
elev = raster::getData("worldclim", var = "alt", res = 2.5) #raster::getData("alt", country = 'USA', res = 2.5)
str(elev)
#"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Makes routes into a spatialPointsDataframe
latlon = na.omit(bbs_latlon)
coordinates(latlon)=c('Lati','Longi')
projection(latlon) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
prj.string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"
# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(latlon, CRS = prj.string) #works now

##### extracting elevation data ####
# A function that draws a circle of radius r around a point: p (x,y)
RADIUS = 40

make.cir = function(p,r){
  points=c()
  for(i in 1:360){
    theta = i*2*pi/360
    y = p[2] + r*cos(theta)
    x = p[1] + r*sin(theta)
    points = rbind(points,c(x,y))
  }
  points=rbind(points,points[1,])
  circle=Polygon(points,hole=F)
  circle
}

#routes.laea@data$dId_site = paste(routes.laea@data$datasetID, routes.laea@data$site, sep = "_")
#routes.laea@data$unique = 1:1003 unnecessary since still have stateroute ID's


#Draw circles around all routes 
circs = sapply(1:nrow(routes.laea@data), function(x){
  circ =  make.cir(routes.laea@coords[x,],RADIUS)
  circ = Polygons(list(circ),ID=routes.laea$stateroute[x]) 
}
)

circs.sp = SpatialPolygons(circs, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# read in elevation raster at 2.5 km resolution (see above)
####below not working, which is STUPID)#### 
NorthAm = rgdal::readOGR(dsn = "C:/Users/mollyfrn/Desktop", layer = "continent")
NorthAm2 = spTransform(NorthAm, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

plot(elevNA2)
plot(NorthAm2)
# Check that circle locations look right
plot(circs.sp, add = TRUE)


####working####
clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}

####not working####
elevNA2 = projectRaster(elev, crs = prj.string) #UNMASKED!
elevNA3 <- raster::mask(elevNA2, NorthAm2)

test = clip(elevNA2, NorthAm2)

elev.point = raster::extract(elevNA3, routes.laea)
elev.mean = raster::extract(elevNA3, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elevNA3, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(unique = routes.laea@data$unique, elev.point = elev.point, elev.mean = elev.mean, elev.var = elev.var)


lat_scale_elev = merge(routes.laea, env_elev, by = c("unique")) # checked to make sure order lined up, d/n seem to be another way to merge since DID keeps getting lost
lat_scale_elev = data.frame(lat_scale_elev)

####Coef vs env variation models####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
uniq_env = unique(bbs_envs[, c('focalrte', 'temp', 'vartemp', 'meanP', 'varP', 'ndvi', 'varndvi', 'elev')])
env_coefs = inner_join(coefs, uniq_env, by = c('stateroute' = 'focalrte'))
covmatrix = round(cor(coefs[, 2:ncol(coefs)]), 2)


# nested loop for examining variation in coefs/fitted curves explained by env vars 
rsqrd_df = data.frame(dep = character(), ind = character(), r2 = numeric())

for (d in 2:25) {
  for (i in 26:ncol(env_coefs)) {
    tempmod = lm(env_coefs[,d] ~ env_coefs[,i])
    tempdf = data.frame(dep = names(env_coefs)[d], 
                        ind = names(env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared)
    rsqrd_df = rbind(rsqrd_df, tempdf)
  }
}
#write.csv(rsqrd_df, "scripts/R-scripts/scale_analysis/mod_rsqrds.csv", row.names = FALSE) #updated 03/28 with elev


####Visually Characterizing r2 vals####
rsqrd_df = read.csv("scripts/R-scripts/scale_analysis/mod_rsqrds.csv", header = TRUE)

ggplot(data = rsqrd_df, aes(x = ind, y = r2, fill = ind))+geom_boxplot()+theme_classic()+
  scale_fill_manual(values = wes_palette("BottleRocket"))+theme(legend.position="none")+
  labs(x = "Environmental variables", y = "Variation Explained (R^2)")

#excluding transient data for incompleteness
rsub_i = rsqrd_df %>%
  filter(dep == "OA.i" | dep == "ON.i" | dep == "CA.i" | dep == "CN.i") %>%
  filter(ind == "elev" | ind == "meanP" | ind == "ndvi" | ind == "temp")
rsub_i = droplevels(rsub_i) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_i, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #what I used for poster w/out color 


#separate analysis for just transients since relationship not immediately apparent
rsub_t = rsqrd_df %>%
  filter(dep == "TAexp" | dep == "TApow" | dep == "TNexp" | dep == "TNpow") %>%
  filter(ind == "elev" | ind == "meanP" | ind == "ndvi" | ind == "temp")
rsub_t = droplevels(rsub_t) #removing ghost levels to ensure correct plotting/analyses

ggplot(data = rsub_t, aes(x = ind, y = r2)) + geom_boxplot()+theme_classic() #elev explains more variation in the transients

####Variance Partitioning of Env Predictors####
#would I be basing my total remaining unexplained variation off of the meanOcc~logA relationship? (OA.i?)
#so the 12% remaining
#focusing just on OA.i and main env vars
#how do variance partitioning with more than 4 parts? 

globalmod<-lm(OA.i~elev+meanP+temp+ndvi, data=env_coefs)
mod1<-lm(OA.i~elev, data=env_coefs)
mod2<-lm(OA.i~meanP, data=env_coefs)
mod3<-lm(OA.i~ndvi, data=env_coefs)
mod4<-lm(OA.i~temp, data=env_coefs)
#and then Euclid_mod2
summary(globalmod)$r.squared
summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared 


#running with mods 2+3 bc best ranked and most interesting 
a= summary(globalmod)$r.squared - summary(mod2)$r.squared
a
c= summary(globalmod)$r.squared - summary(mod3)$r.squared
c
b= summary(mod2)$r.squared - c
b
d= 1- summary(globalmod)$r.squared
d
#isn't it ok that d = ~87.5% tho, given that the r^2 for occ~logA was 88%? 



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


####Env data add-in: Alternate 'cookie-cutter' method for env variables####
#based on Sara's code from summary_and_analysis.R file
# latlongs = read.csv("data/latlongs/latlongs.csv", header =TRUE)
# 
# # merge multiple lat long file to propOcc to get naming convention correct
# latlong_w_sites = merge(latlongs, summ2[,c("datasetID", "site", "propTrans")], by = c("datasetID", "site"), all.x = TRUE) 
# 
# #drop BBS and add in below scale
# latlong_w_sites = subset(latlong_w_sites, !datasetID == 1)
# 
# # reformat non multi grain lat longs
# dft = subset(dataformattingtable, countFormat == "count" & format_flag == 1) # only want count data for model
# dft = subset(dft, !dataset_ID %in% c(1,247,248,269,289,315))
# dft = dft[,c("CentralLatitude", "CentralLongitude","dataset_ID", "taxa")]
# names(dft) <- c("Lat","Lon", "datasetID", "taxa")
# dft2 = merge(dft, summ2[, c("datasetID","site","propTrans")], by = "datasetID")
# 
# # combining all lat longs, including scaled up data
# all_latlongs.5 = rbind(dft2, latlong_w_sites)
# 
# # rbind in new BBS data
# bbs_be_lat = read.csv("data/BBS/bbs_be_lat.csv", header = TRUE)
# # rbind new bbs data to lat longs
# all_latlongs =  rbind(bbs_be_lat, all_latlongs.5)
# all_latlongs = na.omit(all_latlongs)
