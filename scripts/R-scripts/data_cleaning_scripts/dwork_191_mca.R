# Formatting Dataset 191: OBIS Marine Inverts

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:
setwd("C:/Users/auriemma/core-transient/")
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

list.files('data/raw_datasets')

d = read.csv('data/raw_datasets/dataset_191.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(d)
str(d)
summary(d)
dim(d)

# Remove unwanted column 'X'
d = d[,-1]
head(d)

# Rename columns with appropriate names
names(d)= c('datasetID','date','site','species','count')
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore sites
length(unique(d$site))
levels(d$site)

# Sites are formated by stations and lat_longs
# Can remove some info from these sites that is consistent across all of them
d1 = d
site = str_sub(d1$site, start = 16)
head(site, 1000)
tail(site, 1000)
# it worked, so add in new site column
d1$site = site
head(d1,50)

# change back to d
d = d1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore
head(d)
length(unique(d$species))

# Look for bad spp
sp = toupper(d$species)
  # Check for errors caused by letter case
  length(unique(sp))
  # no case errors
unique(sp)

