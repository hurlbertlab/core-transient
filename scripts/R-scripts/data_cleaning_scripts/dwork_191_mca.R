# Formatting Dataset 191: OBIS Marine Inverts

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
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 191 

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

# Change SampleID field to 'site' and ID to datasetID

names(dataset1)[1] = 'datasetID'
names(dataset1)[3] = 'site'

# Check 

head(dataset1)
tail(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample?

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',  'Y') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore

length(unique(dataset1$site))
unique(dataset1$site)

# All sites are listed beginning with 'US_NA_0.1_m^2...', some have other info to follow, and all finish with lat_long data. not sure what other info means but can remove this first part to simplify the data a bit. 

# Make separate new site vector

site = str_sub(dataset1$site, start = 18)
unique(site)

# Add new site field to dataset

dataset2 = dataset1

dataset2$site = factor(site)

head(dataset2)

# Check sites

levels(dataset2$site)

# Several sites still have excess info but lat_longs are last 10-15 characters of each string

head(dataset2, 30)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', "region_catchmethod_sitenumber_lat_long") 

# spatial_scale_variable

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites were listed as North Atlantic regions and site numbers with other unknown information.  Region names were removed to simplify site data, but still plenty of unwanted info in the data.  Lat_long data is included at the end of each data string.)

