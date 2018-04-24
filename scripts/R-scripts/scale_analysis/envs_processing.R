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
library(gimms)
library(devtools)
library(geometry)
library(DBI)
library(RSQLite) #absolutely necessary for NDVI data
library(rdataretriever)
library(magrittr)
library(stringr)
# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
geog = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography/"

#all focal rtes with all possible pairings
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)

#exclude routes that have missing above OR below scale data, such that sites are only calculated for routes that cover all 83 scales
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_latlon = filter(bbs_latlon, stateroute %in% bbs_allscales$focalrte)


sites = data.frame(longitude = bbs_latlon$Longi, latitude = bbs_latlon$Lati) 
#points(sites$longitude, sites$latitude, col= "red", pch=16) #check on map

# Makes routes into a spatialPointsDataframe
latlon = na.omit(bbs_latlon)
coordinates(latlon)=c('Longi', 'Lati')
projection(latlon) = CRS("+proj=longlat +ellps=WGS84") 
#out of order? YUP. 
#had Lati, Longi -> needed to be Longi, Lati. 

prj.string <- CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")
# original in Sara's code: "+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"
# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(latlon, CRS = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) 
#works w/blank projection; doesn't work otherwise
#keep receiving "non finite transformation detected: Lat Longi" error and 
#"error failure in points, 532 projected points not finite 

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

circs.sp = SpatialPolygons(circs, proj4string=CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

# read in elevation raster at 2.5 km resolution (see above)
NorthAm = readOGR(dsn = "//bioark.ad.unc.edu/HurlbertLab/GIS/geography", layer = "continent")
NorthAm2 = spTransform(NorthAm, CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km"))

plot(NorthAm, xlim = c(-160, -60), ylim = c(25, 70))
# Check that circle locations look right #big surprise, they don't -> fix projection!!!
plot(circs.sp, add = TRUE) #looks great

clip<-function(raster,shape) {
  a1_crop<-crop(raster,shape)
  step1<-rasterize(shape,a1_crop)
  a1_crop*step1}


#elev
elev = raster::getData("worldclim", var = "alt", res = 2.5) #raster::getData("alt", country = 'USA', res = 2.5)
str(elev)
elev2 = projectRaster(elev, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
elev3 <- raster::mask(elev2, NorthAm2)

test = clip(elev2, NorthAm2)

elev.point = raster::extract(elev3, routes.laea)
elev.mean = raster::extract(elev3, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elev3, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(routes = routes.laea, 
                      elev.point = elev.point, 
                      elev.mean = elev.mean, elev.var = elev.var)

#write.csv(env_elev, "scripts/R-scripts/scale_analysis/env_elev.csv", row.names = FALSE)

#ndvi 
#gimms
#ndvi_gimms_raw <- get_bbs_gimms_ndvi()
ndvi_gimms_raw = read.csv("data/BBS/ndvi_raw.csv") #Sara version, sourced from tabular data folder 

ndvi_data_summer <- ndvi_gimms_raw %>%
  filter(!is.na(ndvi), month %in% c('may', 'jun', 'jul', 'aug'), year > 2000) %>%
  group_by(site_id, year) %>% #calc avg across summer months for each year
  summarise(ndvi_sum = mean(ndvi), na.rm = TRUE) %>%
  group_by(site_id) %>% #calc avg across years
  summarise(ndvi_mean = mean(ndvi_sum), 
            ndvi_var = var(ndvi_sum), na.rm = TRUE) %>% 
  ungroup()

write.csv(ndvi_data_summer, "scripts/R-scripts/scale_analysis/ndvi_summer.csv", row.names = FALSE) #updated with correct NDVI extraction
#5015 routes with associated ndvi data
#means calculated across 40 km buffer zone, use as would means from other vars 
#write.csv(env_ndvi, "scripts/R-scripts/scale_analysis/env_ndvi.csv", row.names = FALSE)
#updated 05/31 

#precip 
prec = raster::getData("worldclim", var = "prec", res = 2.5)  
prec2 = sum(prec)
prec2 = prec2/1000 #convert to m from mm
plot(prec2) #plotting correctly

prec2 = projectRaster(prec2, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #worked 
#need to reset CRS here bc otherwise spatialpoints objects like routes.laea get coerced to the default CRS of prec2
#prec3 <- raster::mask(prec2, NorthAm2) check with Sara on masking data

prec.point = raster::extract(prec2, routes.laea) #working!!!! 
prec.mean = raster::extract(prec2, circs.sp, fun = mean, na.rm=T)
prec.var = raster::extract(prec2, circs.sp, fun = var, na.rm=T)

env_prec = data.frame(routes = routes.laea, 
                      prec.point = prec.point, 
                      prec.mean = prec.mean, prec.var = prec.var)
#write.csv(env_prec, "scripts/R-scripts/scale_analysis/env_prec.csv", row.names = FALSE) # updated 05/12

#temp 
temp = raster::getData("worldclim", var = "tmean", res = 2.5) 
temp2 = temp/10 #taking to degrees Celsius in correct units
temp3 = mean(temp2) #stack/brick format to layer
plot(temp3)

temp4 = projectRaster(temp3, crs = CRS("+proj=laea +lat_0=45.235 +lon_0=-106.675 +units=km")) #should work, just needs time
#temp3 <- raster::mask(temp2, NorthAm2) #again, check with Sara on skipping this

temp.point = raster::extract(temp4, routes.laea)
temp.mean = raster::extract(temp4, circs.sp, fun = mean, na.rm=T)
temp.var = raster::extract(temp4, circs.sp, fun = var, na.rm=T)

env_temp = data.frame(routes = routes.laea, 
                      temp.point = temp.point, 
                      temp.mean = temp.mean, temp.var = temp.var)
#write.csv(env_temp, "scripts/R-scripts/scale_analysis/env_temp.csv", row.names = FALSE)

####Merge env df's together into one with relevant stateroutes, mean, and var data 
env_elev = read.csv("scripts/R-scripts/scale_analysis/env_elev.csv", header = TRUE)
env_ndvi = read.csv("scripts/R-scripts/scale_analysis/ndvi_summer.csv", header = TRUE)
env_prec = read.csv("scripts/R-scripts/scale_analysis/env_prec.csv", header = TRUE)
env_temp = read.csv("scripts/R-scripts/scale_analysis/env_temp.csv", header = TRUE)

bbs_envs = env_elev %>%
  left_join(env_ndvi, by = c("routes.stateroute" = "site_id")) %>% 
  left_join(env_prec, by = "routes.stateroute") %>%
  left_join(env_temp, by = "routes.stateroute") %>%
  dplyr::select(stateroute = routes.stateroute, 
         elev.mean, elev.var, 
         ndvi.mean = ndvi_mean, ndvi.var = ndvi_var, #I don't have var for ndvi, FIX 09/21
         prec.mean, prec.var, 
         temp.mean, temp.var) %>%
  filter(temp.mean != "NA", prec.mean != "NA", elev.mean != "NA", ndvi.mean != "NA") #945 routes when NA obs removed
write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) 
#current version 09/21


####Pare down routes to exclude routes that are missing above OR below scale####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_envs = filter(bbs_envs, stateroute %in% bbs_allscales$focalrte)

####Calc z-scores, quantiles pre-variance loop####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)

#alt simplistic standardization using z scores 
#means (need for calc top scale variance)
bbs_envs$temp_zm = (bbs_envs$temp.mean - mean(bbs_envs$temp.mean)) / sd(bbs_envs$temp.mean)
bbs_envs$prec_zm = (bbs_envs$prec.mean - mean(bbs_envs$prec.mean)) / sd(bbs_envs$prec.mean)
bbs_envs$elev_zm = (bbs_envs$elev.mean - mean(bbs_envs$elev.mean)) / sd(bbs_envs$elev.mean)
bbs_envs$ndvi_zm = ((bbs_envs$ndvi.mean) - mean(na.exclude(bbs_envs$ndvi.mean))) / sd(na.exclude(bbs_envs$ndvi.mean)) 
#NA's for ndvi vals around statertes in the 3000's
#with z scores for convhull only, don't z scores of variances 

write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) #updated 09/21 to reflect fixed above scale
#conversion done for single-rte scale envs 
#use pre-standardized means to calc above-rte variance and THEN convert to z scores for comparison to lower scales 

####Convex polygon comparison of variables####
#not variances yet 
#notes on geometry package and min convex polygon:
#convhulln from geometry package, optimized by qhull -> convex hull 
#http://www.qhull.org/html/qconvex.htm#synopsis
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_envs.csv", header = TRUE)
#subset to just appropriate dims for convhulln 
#sub_envs = bbs_envs %>% select(temp_zm, prec_zm, elev_zm, ndvi_zm) %>% filter(ndvi_zm != 'NA') #cuts nothing, all there 

# 
# hull = convhulln(sub_envs, "FA")
# hull$area #189.74 #4.502 
# hull$vol #66.22 #0.54 second time around....
# rtes = unique(bbs_envs$stateroute)
# output = c()
# for(s in rtes){
#   sub_envs = bbs_envs %>% filter(stateroute == s)
#   hull = convhulln(sub_envs, "FA")
#   temp = summarize(stateroute = s, 
#                     zhull = hull$vol)  
#   output = rbind(output, temp) 
# } 
#   
#not possible ! need 20 pts! 

#bbs_envs has env data @ scale of single route 
#following code compiles vars of env data @scale of 66 rtes

#ALTERNATIVE: add together z scores as in early code using euclidean distance bet/points or ranking in euclidean space
#- so between scale at 1 and scale at 66 


####Pair env data to secondary rtes associated with each focal rte; calc variance across top scale for each focal rte####
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE) #10/06
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)

#need to pair down by routes existing in bbs_envs (which have been sorted appropriately) and then calculated the top n 66 based on distance
dist.df2 = filter(dist.df, rte1 %in% bbs_envs$stateroute & rte2 %in% bbs_envs$stateroute) 

#num of rows matches num of rows in dts.df, good 
#now calc var for each focal rte (rte1)
top_envhetero = data.frame(stateroute = NULL,
                           scale = NULL, 
                           reg_ndvi_v = NULL,
                           reg_elev_v = NULL,
                           reg_ndvi_m = NULL,
                           reg_elev_m = NULL)

focal_rtes = unique(bbs_envs$stateroute)
#need to structure using focal_clustr template 
scales = c(2:66)
output = data.frame(stateroute = NULL,
                    scale = NULL, 
                    reg_ndvi_v = NULL,
                    reg_elev_v = NULL,
                    reg_ndvi_m = NULL,
                    reg_elev_m = NULL)

#add nu scale component for calculating means at each scale interval 
for(r in focal_rtes){
  for(nu in scales) { #just running for top scale for now 
    #to characterize total var that can be encompassed on a rte
    
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    rte_group = dist.df2 %>% 
      filter(rte1 == r) %>% 
      top_n(nu, desc(dist)) %>%
      dplyr::select(rte2) %>% as.vector()
    
    tempenv = bbs_envs %>%
      filter(stateroute %in% rte_group$rte2)
    
    #can I incorporate an if else conditional here -> 
    #but would only have entry for one row, no, best do it outside
    
    #only want to do this at top scale of 66 rtes, maybe just do sep 
    # tempenv_z = tempenv %>% #subset of tempenv for convhull calcs 
    #   select(temp_zm, prec_zm, elev_zm, ndvi_zm) %>% #using means for convhull calc
    #   filter(ndvi_zm != 'NA') #this is ok since for convhull 
    #   zhull_df = convhulln(tempenv_z, "FA")
    
    #once hull calcs have been done, don't need to keep temp or precip in final df
    
    #get variance of rte means, calc across entire rte_group according to scale   
    temp = data.frame(stateroute = r,
                      scale = nu,
                      reg_ndvi_v = var(tempenv$ndvi.mean, na.rm = TRUE), #fix missing values!!!!
                      reg_elev_v = var(tempenv$elev.mean), #bc each of these values is calculated across the 2ndary rtes for each focal rte
                      reg_ndvi_m = mean(tempenv$ndvi.mean), 
                      reg_elev_m = mean(tempenv$elev.mean)) #a vector is going in and a single val is coming out
    #top_zhull = zhull_df$vol)
    
    output = rbind(output, temp)
  }
  reg_envhetero = rbind(top_envhetero, output) #may need a second level of output rbinding here
}
#removed top scale z score calcs bc not necessary outside of polygon hull calcs 

write.csv(reg_envhetero, "scripts/R-scripts/scale_analysis/reg_envhetero.csv", row.names = FALSE)
#updated 10/06

#naming convention for comparisons: #env.var_zv -> z score origin, variance/habheterogeneity at rt, from 40km buffer circle raster clip.  
#topenv.var_zv -> z score origin, variance/habhet at landscape (var across rt buffermeans in top clustr (zm vector))
#zm from 40 km buffer circle raster clip too

####Coef vs env hetero models####
reg_envhetero = read.csv("scripts/R-scripts/scale_analysis/intermed/reg_envhetero.csv", header = TRUE) #landscape habitat vars 2:66 scale
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_envs.csv", header = TRUE) #single rte habitat vars 1 scale

#Merge top_envhetero to coefs for comparing env variation for a site to its associated AUC 
#at the scale of a landscape and scale of a rte

#also prep datasets for merge 
bbs_envs = bbs_envs[, 1:5] %>% 
  mutate(scale = 1)

reg_envhetero = reg_envhetero %>% 
  rename(elev.mean = reg_elev_m, 
         elev.var = reg_elev_v, 
         ndvi.mean = reg_ndvi_m, 
         ndvi.var = reg_ndvi_v) 

reg_envhetero$scale = as.numeric(reg_envhetero$scale)

env_all = reg_envhetero %>% 
  full_join(bbs_envs) %>% #fixed, but scales out of order with row position, but everything still where it should be 
  arrange(stateroute, scale)

write.csv(env_all, "scripts/R-scripts/scale_analysis/env_all.csv", row.names = FALSE)  
