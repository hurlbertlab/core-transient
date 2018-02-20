##Molly Jenkins 
##09/06/2016 
#Part II: BBS environmental data at multi-stop scales, comparisons with BBC sites 


#Next steps: 
#1) Calculate and assign lat_longs to BBS segments; currently available only for scale of entire BBS routes
#2) Use lat_longs to overlay appropriate NDVI data for each sub-site as proxy for habitat heterogeneity
#3) Test occupancy ~ NDVI for each scale (aes, color = bbs_scalesorted$scale) or a sep panel for each scale 
#4) Use new lat_longs to test minDist between newly segmented BBS sites and BBC sites 
#5) Rerun geog and env distance analysis for multiscale comparison 

bbs_stops<-read.table("//bioark.ad.unc.edu/hurlbertlab/Databases/BBS/GPS_stoplocations/May_2014_All_stops.txt", sep = ",", header = TRUE, fill = TRUE)
#line 6931 doesn't have 7 elements (missing data!) so setting fill = TRUE in meantime
#MUST add other point data!! This is clearly just for Canada and does not include the US stop data
#Intermediate step: download shapefiles of bbs routes and approximate + extract point segment data upon return using a GIS 


#First: paste "Stop" in front of stop #'s in bbs_stops$Stop
bbs_stops$Stop = paste("Stop", bbs_stops$Stop, sep = "")

#as it stands, assigned stop coordinates refer only to the endpoint of a segment
#and they do not comprise the location of the entire segment itself 
#any distance measurements will be based off of the endpoint of a stop segment, and not a central point
#do we just want to measure distance from that endpoint stop location to the nearest BBC route? 







#merge by stateroutes and subrouteID from bbs_scalesorted 
#by Province_Route and Stop from bbs_stops ** going with these names
#Not all of the stops at each route will have lat_longs, and so these rows will need to be removed 
#first rename columns in one dataframe to match the columns in the other 
names(bbs_scalesorted)[2] = "Province_Route"
names(bbs_scalesorted)[4] = "Stop"
#next, merge: 
bbs_coords<-merge(bbs_scalesorted, bbs_stops, by= c("Province_Route", "Stop"), all=T)


#remove unnecessary columns like X, route, active, and province 
bbs_coords = bbs_coords[,-3]
bbs_coords = bbs_coords[,-6]
bbs_coords = bbs_coords[,-9]
bbs_coords = bbs_coords[,-8]

#remove rows with no POINT_X and POINT_Y coords or no occ and AOU info 
bbs_coords = na.omit(bbs_coords)


#2) add NDVI data using lat/longs 
#first plot newly broken up bbs stop segment points out as sites
sites<-data.frame(longitude = bbs_coords$POINT_X, latitude = bbs_coords$POINT_Y)
points(sites$longitude, sites$latitude, col= "red", pch=16)


#NDVI 
ndvimean<-raster("//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/Vegetation_Indices_may-aug_2000-2010.gri")
plot(ndvimean)
points(sites$longitude, sites$latitude, col = 'red', pch = 16)
test3 = extract(ndvimean, sites)
head(test3)
ndvimean<-ndvimean/10000
bbs_coords$ndvi<-extract(ndvimean, sites)





ndvimod = lm(occupancy ~ ndvi, data = bbs_coords)
summary(ndvimod)
#need to subset and rerun by scale






#Incorporate BBC data and pair by minimum distance to stop segments by nearest segment endpoint 

#----Read in BBC data for both counts and sites (currently not seperated by year) (see Ethan White pdf-transformation code)----
bbc_counts<-read.csv("bbc_counts.csv", header=TRUE)
head(bbc_counts)
bbc_sites<-read.csv("bbc_sites.csv", header=TRUE)
head(bbc_sites)
#----For BBC site data: prepare for site-pairing by subsetting assigning headers "BBC Site", "Lat", "Long"----
bbc_lat_long<-unique(subset(bbc_sites, select=c("siteID", "latitude", "longitude")))
bbc_lat_long$longitude= -bbc_lat_long$longitude
head(bbc_lat_long)
length(table(bbc_lat_long$siteID)) #should be 360
#----Write for_loop to calculate distances between every BBS and BBC site combination to find sites and routes that correspond best----
#store minimum value for each iteration of in output table
require(fields)
#calculate distances using Great Circle Distance equation
output=c()
for(bbc in bbc_lat_long$siteID){
  temp.lat=bbc_lat_long$latitude[bbc_lat_long$siteID==bbc]
  temp.lon= bbc_lat_long$longitude[bbc_lat_long$siteID==bbc] 
  distances = rdist.earth(matrix(c(bbs_coords$POINT_X, bbs_coords$POINT_Y), ncol=2),matrix(c(temp.lon,temp.lat), ncol=2),miles=FALSE, R=6371)
  minDist= min(distances)
  closestBBS=bbs_coords$Province_Route[distances==minDist] #need to combine province route and stop # for unique ID 
  #unique ID = within-route ID that specifies both route and stop # endpoint
  output=rbind(output, c(bbc, closestBBS, minDist))
}
output = as.data.frame(output)
colnames(output)<-c("bbcsiteID", "bbsrouteID", "minDist")
head(output)
summary(output)

