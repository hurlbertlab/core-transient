####Supp figures: comparing raw and core_coef patterns between 3 different cutoff vals for core species####
# Alternate cutoffs of core and transient species in a community: are distributions changed?
# author: Molly F. Jenkins
# date: 04/12/2018

#The following script demonstrates 2 alternatives to the original cutoffs of >2/3 and <1/3, which can be viewed in the pctcore_alt.R script. 
#This script explores cutoffs of: 
#2/3 $ 1/3 (original occ-scale-processing and creation of bbs_allscales.csv data)
#2/4 & 2/4 
#4/5 & 1/5 

#This scripts pulls in bbsallscales, bbsallscales_50, and bbs_allscales80, as well as 
#core_coefs, core_coefs_50, and core_coefs_80 for visual comparisons

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
library(viridis)

# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'


####Merge 3 core_coefs versions, add new factor column variable delineating cutoff category####
core_coefs67 = read.csv("scripts/R-scripts/scale_analysis/core_coefs.csv", header = TRUE) 
core_coefs50 = read.csv("scripts/R-scripts/scale_analysis/core_coefs50.csv", header = TRUE) 
core_coefs80 = read.csv("scripts/R-scripts/scale_analysis/core_coefs80.csv", header = TRUE) 

core_coefs50 = core_coefs50 %>% 
  mutate(cutoff_lvl = "Core 50%") #1

core_coefs67 = core_coefs67 %>% 
  mutate(cutoff_lvl = "Core 67%") #2

core_coefs80 = core_coefs80 %>% 
  mutate(cutoff_lvl = "Core 80%") #3

coefs_supp_pre = rbind(core_coefs50, core_coefs67) # could probably rbind or inner join, idk 
coefs_supp = rbind(coefs_supp_pre, core_coefs80)

coefs_supp$cutoff_lvl = factor(coefs_supp$cutoff_lvl, 
                         levels = c("Core 50%","Core 67%", "Core 80%"),
                         labels = c("Core 50%","Core 67%", "Core 80%"))

#gather 

coefs_tidy = coefs_supp %>% 
  gather(key = parm,
         value = parm_val, 
         2:11)

write.csv(coefs_tidy, "scripts/R-scripts/scale_analysis/coefs_tidy_supp.csv", row.names = FALSE)

####Supplemental figures: plotting differences in coef vals at diff cutoffs####


ggplot(coefs_tidy, aes(x = cutoff_lvl, y = parm_val))+
  facet_wrap(~parm)+geom_boxplot() #, labeller = label_parsed)



# coefs_tidy$parm = factor(coefs_tidy$parm, 
#                          levels = c("PCA.min","PCA.mid", "PCA.slope","PCA.curvature", "PCA.max"),
#                          labels = c(as.character(expression("p"["min"])), as.character(expression("Scale"[50])), "Slope", "Curvature", as.character(expression("p"["max"]))))



