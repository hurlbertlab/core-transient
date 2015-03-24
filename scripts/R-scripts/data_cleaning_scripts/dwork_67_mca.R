# Formatting Dataset 67: OBIS Birds

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

# Source the functions file:

getwd()

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 67 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(dataset)
str(dataset)
head(dataset)

# Remove unused field labeled 'X'
dataset1 = dataset[,-1]
head(dataset1)

# Change SampleID field to 'site'
names(dataset1)[3] = 'site'

#check 

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore
length(unique(dataset1$site))
unique(dataset1$site)

# 423 sites listed as "South_Africa_Lat_Long"
# Can remove the 'South Africa' portion leaving just lat-longs

site = str_sub(dataset1$site, start = 14)

# Check for accuracy

unique(site)
length(dataset1$site)
length(site)

# All good, so add new site field to dataset

dataset2 = dataset1

dataset2$site = factor(site)

head(dataset2, 30)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

