####Environmental variable and lat-lon breakdown of abundance and occupancy dist across sites####
#set working directory e.g. setwd("C:/git/core-transient/")
#bring in routes with lat lons
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route
routes$focalrte = routes$stateroute
routes = routes %>% 
  dplyr::select(focalrte, Lati, Longi)
#bring in occ data
bbs_focal_occs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", header = TRUE)

#pair occ data to lat lons by focal routes 
test_merge = bbs_focal_occs %>% 
  left_join(routes, bbs_focal_occs, by = "focalrte") 

#bring in env data by lat lons 
sites = test_merge %>% 
  dplyr::select(Longi, Lati)

points(sites$Longi, sites$Lati, col = 'red', pch = 16)
sites<-data.frame(longitude = sites$Longi, latitude = sites$Lati)

####Temp####
files = paste('//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/tmean',1:12,'.bil', sep='')
tmean = stack(files) 
plot(tmean)

#above code works!!!

# Find MEAN across all months
meanT = calc(tmean, mean)
meanT

# Convert to actual temp
meanT = meanT/10 #done

temp_sites<-extract(meanT, sites)

####Precip####
precip<-paste('//bioark/HurlbertLab/GIS/ClimateData/2-25-2011/prec/prec_',1:12,'.bil', sep='')
pmean = stack(precip) 
meanP = calc(pmean, mean)
precip<-extract(meanP, sites)

####NDVI####
ndvimean<-raster("//bioark/HurlbertLab/GIS/MODIS NDVI/Vegetation_Indices_may-aug_2000-2010.gri")
ndvi <-extract(meanNDVI, sites)



