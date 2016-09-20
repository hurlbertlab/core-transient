#Variation in occupancy at multiple scales WITHIN BBS sites
#Molly F. Jenkins 
#07/27/2016

#Set working directory to core-transient folder on github i.e. setwd("C:/git/core-transient/")


## Please download and install the following packages:
# maps, sp, rgdal, raster, maptools, rgeos
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)


#fifty = ecoretriever::fetch('BBS50')
#bbs50 = fifty
#bbs50 = bbs50$counts
#bbs50$stateroute = bbs50$statenum*1000 + bbs50$Route
#bbs50$stateroute = as.integer(bbs50$stateroute)
#^derivation of data from ecoretriever; still too large to host on github so pull from BioArk

bbs50 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs50.csv", header = TRUE)

# Get subset of BBS routes (just routes) btw 1996-2010 surveyed in EVERY year

#from Sara's code
good_rtes = bbs50 %>% 
  filter(year >= 1996, year <= 2010) %>% 
  select(year, stateroute) %>%
  unique() %>%    
  group_by(year) %>% 
  count(stateroute) %>% 
  filter(n == 15) #strange discrepancy between method needed on home laptop and on lab desktop...update R on both, make sure same version


# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 1996, year <= 2010) %>% 
  filter(stateroute %in% good_rtes$stateroute) #finally works because needed $ specification, 
#can probably collapse into one line 
 
#write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/filteredrtes.csv")
#wrote to file just in case 


###So for the whole dataset, 10 pt count stops: #we are only getting one out of five chunks along 
#want to estimate occupancy across each one, as of now only estimating for count 10 column 
#fifty pt count data and then taking pts 1-5 and collapsing them all together 
#########
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) #because this gets rid of 0's...
  bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15)
  bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,] #and this also gets rid of occupancy values of 0 total 
  names(bbsu.rt.occ2)[3] = "occupancy"
  bbsu.rt.occ2$subrouteID = countColumns[1] #subrouteID refers to first stop in a grouped sequence, occ refers to the occ for the # of combined stops
  bbsu.rt.occ2$scale = scale 
  bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "scale", "subrouteID", "AOU", "occupancy")]
  return(bbsu.rt.occ2)
}

# Generic calculation of occupancy for a specified scale

scales = c(5, 10, 25, 50)


output = c()
for (scale in scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts(fifty_allyears, groupedCols, scale)
    output = rbind(output, temp)
  }
  
}

bbs_scalesorted2<-output


####Jes Coyle's MD scale analysis reference script#### 

counts5 = read.csv('data/raw_datasets/dataset_1RAW/dataset_1_full.csv', header=T)
occupancy.matrix = as.matrix(read.csv('scripts/R-scripts/scale_analysis/occ_matrix_BBS.csv', header=T, row.names = 1))

# MD BBS data
md.counts = subset(counts5, statenum==46) #sub to MD
md.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==46,]
md.uniq = unique(md.counts[,c('Year','Aou')])
# MD statewide temporal occupancy (27 routes)
md.occ = data.frame(table(md.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
md10 = unique(md.counts[md.counts$Count10!=0,c('stateroute','Year','Aou')])
md10.rt.occ = data.frame(table(md10[,c('stateroute','Aou')])/15)
md10.rt.occ2 = md10.rt.occ[md10.rt.occ$Freq!=0,]



#####Testing occupancy of old vs occupancy of new for consistency####
y = bbs_scalesorted2 %>% 
  filter(subrouteID == "Stop1" & scale == 10) %>% 
  filter(stateroute %in% md10.rt.occ2$stateroute) %>% #47 items when aou not limited, interesting
  filter(AOU %in% md10.rt.occ2$Aou)

#42 items


x = md10.rt.occ2 %>% #we know that these are already just the scale 10 sites from stops 1-10 in MD 
  filter(stateroute %in% y$stateroute) #42 items as well



plot(x$Freq, y$occupancy, type = "l", main = "MD old occupancy vs new",  ylab = "new", xlab = "old") #plots linear? YES awesome perfectly linear for MD subset



##Test again for CA/OR (west coast sample!) just in case 

ca.counts = subset(counts5, statenum==14 | statenum == 69)
ca.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==14 |
                                floor(as.numeric(row.names(occupancy.matrix))/1000)==69,]
ca.uniq = unique(ca.counts[,c('Year','Aou')])
# CA/OR statewide temporal occupancy (27 routes)
ca.occ = data.frame(table(ca.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
ca10 = unique(ca.counts[ca.counts$Count10!=0,c('stateroute','Year','Aou')])
ca10.rt.occ = data.frame(table(ca10[,c('stateroute','Aou')])/15)
ca10.rt.occ2 = ca10.rt.occ[ca10.rt.occ$Freq!=0,]

y2 = bbs_scalesorted2 %>% 
  filter(subrouteID == "Stop1" & scale == 10) %>% 
  filter(stateroute %in% ca10.rt.occ2$stateroute) %>% # items when aou not limited, interesting
  filter(AOU %in% ca10.rt.occ2$Aou)

#58 items


x2 = ca10.rt.occ2 %>% 
  filter(stateroute %in% y2$stateroute) %>% #60 items? 
  filter(Aou %in% y2$AOU) #58 items


plot(x2$Freq, y2$occupancy, main = "CA/OR old occupancy vs new",  ylab = "new", xlab = "old") #there are differences for the west coast data, womp 


#want to compare occupancy to scale 
scale_occ_mod = lm(occupancy~scale, data = bbs_scalesorted2)
summary(scale_occ_mod)


#scale at even level of within a bbs stop *MATTERS* 
#so what happens when we split up data by scale visually, a 10scale, etc and run these by occupancy? 


fig_one<-boxplot(occupancy ~ scale, data = bbs_scalesorted2, xlab = "Scale", ylab = "BBS Occupancy")

#seeing the same patterns with new occupancy calculations

# -----------------------------------------------------------
####Calculating occupancy at scales greater than a single route####

# bring in bbs routes file 
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


# merge lat longs from routes file to the list of "good" routes

good_rtes = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") 

# map these routes
# need North American map base first -> modified from "dataset_map.R" script as reference

par(mfrow=c(1,1), mar=c(0,0,0,0))
cex.terr = 1.3

map('world',xlim=c(-165,-55),ylim=c(25,70), bg='black', fill=T, col='white')
map('state',add=T)

sites<-data.frame(longitude = good_rtes$Longi, latitude = good_rtes$Lati)
points(sites$longitude, sites$latitude, col= "red", pch=16)


# figure out how many routes are present in grid cells of varying size

# count how many there are per grid cell at different scales

# e.g. doing this for both lat & long

####prototype forloop for generating scaled-up samples for calculating occupancy####

grains = c(1, 2, 10)


output = c()
for (grain in grains) {
  lats = 100*runif(50)
  for (l in 1:lats) {
    groupedCols = paste("Rt_group", floor(lats/grain)*grain + grain/2, sep = "")
    temp = occ_counts(fifty_allyears, groupedCols, grain)
    output = rbind(output, temp)
  }
  
}

bbs_scaledup = output



####reference code for calculating grain and random selection of routes by lat
grain = 1
lats = 100*runif(50)
floor(lats/grain)*grain
#below is example of 50 random floored lats generated at grain 1, 
#where grain is analagous to the scales vector from before?

#[1] 10 21 15 86 24 96 47 16 41  
#9 24 64 63 54 11 78 66 83 32 10 85 51 
#34 26 95 77 55 85 14 64 62 85  
#6 54 59 41 22 40 84 93  3
#[42] 18 29 82  2 68  8 94 58 96


floor(lats/grain)*grain + grain/2
