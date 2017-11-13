####Secondary analysis to Jenkins Core-Transient Occupancy Scaling Project####
# author: Molly F. Jenkins
# date: 11/12/2017
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
library(geometry)
# To run this script, you will need BBS & BBC data, 
# which are currently stored in the following directories off of github: 

# Data directories
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'
BBC = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBC' 


#read in data 

####Tidy BBC data####
#unique combos of siteID, lat & longitude 

#ensure creation of "breeder_status" column in BBC stating whether core, transient, or missing


#Calc dists between BBC and nearest corresponding BBS sites; arrange in ascending order 
#limit to max dist of 40km in order for two sites to qualify for pairing 


##Match spp names from AOU codes in BBS to common names in BBC data 

##Merge datasets and create "missing" category for spp present in one dataset but not other 

####ANALYSIS####
#I: How well does BBS occupancy correspond to BBC status in general?
fig_one<-boxplot(occupancy~status, data=tidy_whole, xlab="BBC Status", ylab= "BBS Occupancy")
fig_one$stats


#II: Site-by-site basis from a BBS perspective: 
#do the BBC and BBS designations agree or disagree?
corrsp=c()
uniqueBBSrts<-unique(tidy_whole$bbsrouteID) 

for (bbs in uniqueBBSrts) { 
  temp.site=subset(tidy_whole, bbsrouteID == bbs) #alternatively have tried temp.site=tidy_whole$bbsrouteID[tidy_whole$bbsrouteID==bbs]
  BBS_Core_agreement = sum(temp.site$occupancy>0.67 &
                             temp.site$status=="breeder", 
                           na.rm = T)/sum(temp.site$occupancy>0.67, na.rm = T)
  BBS_Trans_agreement = sum(temp.site$occupancy<0.33 & 
                              (temp.site$status=="visitor" | temp.site$status=="missing"),
                            na.rm = T)/sum(temp.site$occupancy<0.33, na.rm = T)
  BBS_Core_disagreement = sum(temp.site$occupancy>0.67 &
                                (temp.site$status=="visitor" | temp.site$status=="missing"),
                              na.rm = T)/sum(temp.site$occupancy>0.67, na.rm = T)
  BBS_Trans_disagreement = sum(temp.site$occupancy<0.33 & temp.site$status=="breeder", 
                               na.rm = T)/sum(temp.site$occupancy<0.33, na.rm = T)
  
  corrsp=rbind(corrsp, 
               c(bbs, BBS_Core_agreement, BBS_Core_disagreement, 
                 BBS_Trans_agreement, BBS_Trans_disagreement))
  }
corrsp = as.data.frame(corrsp)
names(corrsp)<-c("BBSrouteID", 
                  "Core_Core",  
                  "Core_Trans", 
                  "Trans_Trans", 
                  "Trans_Core")

#Figures
fig_two<-boxplot(corrsp[, 2:5], xlab="Dataset Agreement", ylab= "Frequency")
fig_two$stats

#plot agreement vs distance apart
#merge minDist to corrsp by bbsrouteID; do I want to merge by independent distance values? 
#should I clump agreements together? how do I want to categorize it? 
#read in minDist: 


distance_agreement<-merge(minDist, corrsp,  by.x ="bbsrouteID", by.y="BBSrouteID")
fig_three<-boxplot(distance_agreement[4, 9:12], xlab="Dataset Agreement", ylab="minDist")


#calculate frequency averages for agreement pairings
frequency_avgs<-colMeans(corrsp)
frequency_avgs

#Part III: Model environmental variation/habhet w/core-trans designations, occupancy----
#read in env BBS data 

mod_data<-merge(corrsp, env_data, by.x="BBSrouteID", by.y="stateroute")
mod_data2<-merge(tidy_whole, env_data, by.x="bbsrouteID", by.y="stateroute")


#models: 
core_agreement_mod<-lm(Core_Core~sum.NDVI.mean, data=mod_data)
core_disagreement_mod<-lm(Core_Trans~sum.NDVI.mean, data=mod_data)
trans_agreement_mod<-lm(Trans_Trans~sum.NDVI.mean, data=mod_data)
trans_disagreement_mod<-lm(Trans_Core~sum.NDVI.mean, data=mod_data)

mod1<-lm(occupancy~sum.NDVI.mean, data=mod_data2)
summary(mod1)
mod2<-lm(occupancy~elev.mean, data=mod_data2)


summary(core_agreement_mod)
summary(core_disagreement_mod)
summary(trans_agreement_mod)
summary(trans_disagreement_mod)
