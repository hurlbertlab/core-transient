#Molly F. Jenkins 
#10/24/2016 
#Core-Transient Scale Analysis: Above-route analyses for best sampling practice 

#This script was developed to aid in determining the best # of routes to sample for each grid cell size across BBS data. 
#It is also an active script for developing a "moving window" approach to grid cells. 
#Eventual output should be a table with a "grain" column and a corresponding "sample_size" column
#This table will replace the current "grain_sample" table in the occ_dist_vs_scale.R script. 


#-----------------------------------------------------------------------------------------
#Molly's code (for deriving suggested paired grains and # of stateroutes sampled in each grain)

####grid sampling justification####

#bringing in lat lon associated with each route (so can determine routes present in grid cell)
good_rtes = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", header = TRUE)
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


#putting these files together to get routes present in all years 2000-2014 
#AND their associated lat-lon data 
require(dplyr)
stateroute_latlon = routes %>% 
  filter( routes$stateroute %in% good_rtes$stateroute) %>%  
  dplyr::select(stateroute, Lati, Longi) 
#write.csv(stateroute_latlon, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/stateroute_latlon.csv", row.names = FALSE)


#setting grain to largest grid cell size 
sampledgrains = data.frame(c(1, 2, 4, 8))
names(sampledgrains) = c("grain")
output = c()
for (grain in sampledgrains$grain) {
  #binning stateroutes according to latlon in grid8ID cells
  stateroute_latlon$latbin = floor(stateroute_latlon$Lati/grain)*grain + grain/2 
  stateroute_latlon$longbin = floor(stateroute_latlon$Longi/grain)*grain + grain/2
  stateroute_latlon$gridID = paste(stateroute_latlon$latbin, stateroute_latlon$longbin, sep = "")
  stateroute_latlon$grain = grain
  output = rbind(output, stateroute_latlon)
  
}

sample_sizes = output 
unique(sample_sizes$grain)
write.csv(sample_sizes, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/sample_sizes.csv", row.names = TRUE)
#cool, developed forloop appropriate, now can calc grid rte totals FOR EACH GRAIN 
#and take top 6 for each, find min of top six and use as "magic number"


sample_sizes = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/sample_sizes.csv", header = TRUE)
#count # of stateroutes in each cell, take top 6 cells (for both sub and above-route occupancy)
require(dplyr) 
grid_rte_totals_1 = sample_sizes %>% 
  filter(grain == 1)  %>%
  count(gridID) %>% 
  arrange(desc(n))

grid_rte_totals_2 = sample_sizes %>% 
  filter(grain == 2)  %>%
  count(gridID) %>% 
  arrange(desc(n))

grid_rte_totals_4 = sample_sizes %>% 
  filter(grain == 4)  %>%
  count(gridID) %>% 
  arrange(desc(n))

grid_rte_totals_8 = sample_sizes %>% 
  filter(grain == 8) %>%
  count(gridID) %>% 
  arrange(desc(n))


#it works! now find how many cells can include when in each grain set     

top6_grid8 = as.character(unique(head(grid_rte_totals_8$gridID)))
write.csv(top6_grid8, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/top6_grid8.csv", row.names = FALSE)
#take head of EACH    
#top 6 cells to cut off sample at 

# grain of (8, 4, 2, 1) corresponds to suggested samples of 66, 31, 14, 6 rtes in each sample

####Fromerly part of alt_occ_scale.R####

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)
fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)

require(dplyr)
bbs50_goodrtes = inner_join(fifty_allyears, good_rtes2, by = "stateroute")
#ID and subset to routes within top six regions id'd in grid_sampling_justification 
bbs50_goodrtes$grid8ID = paste(floor(bbs50_goodrtes$Lati/8)*8 + 8/2, floor(bbs50_goodrtes$Longi/8)*8 + 8/2, sep = "")
bbs50_goodrtes$grid8ID = as.character(bbs50_goodrtes$grid8ID)


#bring in top 6 grids for max scale (8 degree) from grid_sampling_justification.R script, 66 rte cutoff 
top6_grid8 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/top6_grid8.csv", header = TRUE)


#filter the 50 stop data to just those routes present within those 6 grid cell regions of interest 
fifty_top6 = bbs50_goodrtes %>% 
  filter(grid8ID %in% top6_grid8$x) %>%
  dplyr::select(7:62)#about halves the bbs50_goodrtes set of usable routes, and no redundant columns
#write.csv(fifty_top6, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_top6.csv", row.names = FALSE)


#######

fifty_top6 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_top6.csv", header = TRUE)



#-----------------------------------------------------------------------------------------
#Allen Hurlbert's code (for mapping distribution of stateroutes sampled by grain size, 
#for visually refining samples suggested, based on East-West distribution of sites)

####Determining ideal magic number "X" assigned to each grain; creating this file to draw from to use in below
#in lieu of hardcoding grain and sample_n portions prior to loops 

good_rtes = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", header = TRUE)

#bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)

# bring in bbs routes file 
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


# merge lat longs from routes file to the list of "good" routes
require(dplyr)
temproutes = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)

ct= c()

map_threshold = function(grain, thresh) {
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  
  ct = temproutes %>% 
    count(latbin, longbin)
  
  map('state')
  points(ct$longbin[ct$n >= thresh], ct$latbin[ct$n >= thresh], 
         cex = log10(ct$n[ct$n >= thresh]), pch = 16)
  leg_benchmarks = c(2, max(ct$n)/2, max(ct$n))
  legend("bottomright", legend = c(2, max(ct$n)/2, max(ct$n)), pch = 16,
         pt.cex = log10(leg_benchmarks))
 
}


map_threshold(8, 66) 

#scale 1, sample 5 
#scale 2, sample 10 
#scale 4, sample 19 (adds 4 western locations)
#scale 8, sample 66 

text(ct$longbin, ct$latbin, ct$n)

####Adding grid dimensions to map for explaining grid cell binning visually in methods####
#library
library(leaflet)

# Create 20 markers (Random points)
data=data.frame(long=sample(seq(-150,150),20) ,  lat=sample(seq(-50,50),20) , val=round(rnorm(20),2) , name=paste("point",letters[1:20],sep="_")  ) 

# Show a rectangle
m=leaflet() %>% addTiles() %>%  
  addRectangles(
    lng1=-72, lat1=48,
    lng2=-80, lat2=40,
    fillColor = "transparent"
  )
m



hist(ct$n)
median(ct$n)

quantile(ct$n, 0.6)


####Map figures for Methods section#### 
