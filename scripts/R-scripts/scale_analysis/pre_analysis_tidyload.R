#Molly F. Jenkins 
#updated 05/03/2018
####Pre-analysis prep and partitioning of BBS50 and BBS(non-50 stop) data####

#I set this portion of the data cleaning into its own script so that it would be easier to keep track of 
#where goodrtes etc. were being derived from, as well as which dataset corresponded to 50 vs non-50 stop 

#Set working directory to core-transient folder on github i.e. setwd("C:/git/core_scale")
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
library(rdataretriever)

BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'

####Bringing in BBS50 stop data and prepping it for sub-route scale partitioning####
#check name of bbs 50 stop data and correct query if necessary 
rdataretriever::datasets()
bbs = rdataretriever::fetch('breed-bird-survey-50stop') #pull in entire bbs database for 50 stop res data
bbs50 = bbs$breed_bird_survey_50stop_counts  #focus in on just the raw counts data 
bbs50$stateroute = bbs50$statenum*1000 + bbs50$route
bbs50$stateroute = as.integer(bbs50$stateroute)
#^derivation of data from ecoretriever; saved in bioark 
write.csv(bbs50, "intermed/bbs50.csv", row.names = FALSE)  #includes 2015

routes = bbs$breed_bird_survey_50stop_routes
#file of ALL BBS routes for N. Am. with Lat Longs
routes$stateroute = routes$statenum*1000 + routes$route


# Get subset of BBS routes (just routes) btw 2000-2014 surveyed in EVERY year
require(dplyr)
#from Sara's code
good_rtes = bbs50 %>% 
  filter(year >= 2000, year < 2015) %>% #shifted 15 year window to 2000-2014 bc that's where 1005 happens
  select(year, stateroute) %>%
  unique() %>%    
  group_by(stateroute) %>%  
  count(stateroute) %>% 
  filter(n == 15) #now getting 1005 routes with consecutive data :^)
write.csv(good_rtes, "intermed/good_rtes.csv", row.names = FALSE)

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year < 2015) %>% 
  filter(stateroute %in% good_rtes$stateroute)
write.csv(fifty_allyears,"intermed/fifty_allyears.csv", row.names = FALSE)
#50 stop data, updated/checked 05/03/2018


# merge lat longs from routes file to the list of "good" routes (2001-2015 present all years)
require(dplyr)
good_rtes2 = routes %>% 
  filter(stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(stateroute, latitude, longitude)
write.csv(good_rtes2, "intermed/good_rtes2.csv", row.names = FALSE)
#updated 05/03/2018

