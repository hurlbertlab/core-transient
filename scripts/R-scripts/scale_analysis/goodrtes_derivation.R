#Molly F. Jenkins 
#01/19/2016
####Pre-analysis prep and partitioning of BBS50 and BBS(non-50 stop) data####

#I set this portion of the data cleaning into its own script so that it would be easier to keep track of 
#where goodrtes etc. were being derived from, as well as which dataset corresponded to 50 vs non-50 stop 

#Set working directory to core-transient folder on github i.e. setwd("C:/git/core-transient/")
#years are 2000-2014 because 2001-2015 only provides 953 rtes; vs 2000-2014 provides 1005 routes

## Please download and install the following packages:
# maps, sp, rgdal, raster, maptools, rgeos, dplyr, fields
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)
library(ecoretriever)

####Bringing in BBS50 stop data and prepping it for sub-route scale partitioning####
# bbs50 = ecoretriever::fetch('BBS50')
# bbs50 = bbs50$counts
# bbs50$stateroute = bbs50$statenum*1000 + bbs50$Route
# bbs50$stateroute = as.integer(bbs50$stateroute)
#^derivation of data from ecoretriever; saved in bioark 
bbs50 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/bbs50.csv", header = TRUE) #includes 2015

routes = read.csv("C:/git/core-transient/scripts/R-scripts/scale_analysis/routes.csv", header = TRUE)
#file of ALL BBS routes for N. Am. with Lat Longs
routes$stateroute = routes$statenum*1000 + routes$Route


# Get subset of BBS routes (just routes) btw 2000-2014 surveyed in EVERY year
require(dplyr)
#from Sara's code
good_rtes = bbs50 %>% 
  filter(year >= 2000, year < 2015) %>% #shifted 15 year window to 200-2014 bc that's where 1005 happens
  select(year, stateroute) %>%
  unique() %>%    
  group_by(stateroute) %>%  
  count(stateroute) %>% 
  filter(n == 15) #now getting 1005 routes with consecutive data :^)
#write.csv(good_rtes, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", row.names = FALSE) 

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year < 2015) %>% 
  filter(stateroute %in% good_rtes$stateroute)

#finally works because needed $ specification, 
#can probably collapse into one line 
#write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", row.names = FALSE)
#wrote to file just in case 

# merge lat longs from routes file to the list of "good" routes (2001-2015 present all years)
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)
#write.csv(good_rtes2, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/good_rtes2.csv", row.names = FALSE)
#updated 05/04/2017

