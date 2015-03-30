# Formatting Dataset 124: OBIS Marine Inverts

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
setwd('C:/Users/auriemma//core-transient')
source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 124 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
str(dataset)
summary(dataset)
tail(dataset)

# Remove column called 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Change ID column to 'datasetID'

names(dataset1)[1] = 'datasetID'

# Change name of site column

names(dataset1)[3] = 'site'

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','Y') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore

head(dataset1)
length(unique(dataset1$site))
summary(dataset1)

# Check all sites

levels(dataset1$site)

# Sites are listed with a name (USA_Massachusetts or USA_Massachusetts_North_America_Atlantic) followed by a lat_long. Data with North_America_Atlantic also includes a number before the lat_long. Can get rid of the redundant 'USA_Massachusetts' so that at least some of the data will be just lat_longs.

dataset2 = dataset1
dataset2$site = str_sub(dataset2$site, start = 21)

# Check

head(dataset2,100)
unique(dataset2$site)

# All looks good; sites are now either just lat_long or read as 'rth_America_Atlantic_number_lat_long

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit','lat_long or site number and lat_long') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N') 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', "site fields were cut down using substring to delete the redundant site name USA_Massachusetts which was present in every site. Remaining are the sites as just lat_longs and also sites with rth_America_Atlantic followed by possibly a site number, then a lat_long")

