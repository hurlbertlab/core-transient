# Formatting Dataset 99: OBIS Tropical Fish

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
setwd("C:/Users/auriemma/core-transient/")
source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 99

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)
head(dataset)
tail(dataset)
summary(dataset)

# Remove unused column 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Rename SampleID as site, and ID as datasetID

names(dataset1)[c(1,3,4)] = c('datasetID','site','species')

# Check

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
summary(dataset1)

# Sites give a lot of information like surveyor, method, station number, dates
# Can't find metadata to see what is relevant data

# No changes to be made to site field

dataset2 = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'surveyor_method_station_sitenumbers_date_other') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'Y') 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'Sites give a lot of information like surveyor, method, station number, dates, etc.  Not sure what data should be used because no metadata info found. No changes made to site field data.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore

head(dataset2, 30)
length(unique(dataset2$species))

# Uppercase species

dataset2$species = factor(toupper(dataset2$species))
length(unique(dataset2$species))

# Look through all species for bad species

levels(dataset2$species)
species = levels(dataset2$species)
species[1:500]
species[500:1079]

# No bad spp found

head(dataset2)
dataset3 = dataset2

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. 

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'No bad species found.  No changes made to field.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore

names(dataset3)

# Fill in the original field name here

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# Check for zeros and NAs

summary(dataset3)

# No zeros, remove NAs

dataset5 = na.omit(dataset3)

# No zeros or NAs in data

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', "Data represents abundance counts. no changes or removals necessary")

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*