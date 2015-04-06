# Formatting Dataset 120: OBIS Benthos

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

ds = 120 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
summary(dataset)

# Remove field labeled 'x'

dataset1 = dataset[,-1]

# Check 

head(dataset1)

# change ID to datasetID

names(dataset1)[1] = 'datasetID'

# change SampleID to site

names(dataset1)[3] = 'site'

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

length(unique(dataset1$site))
unique(dataset1$site)

# Sites give the region of site, number, and lat_longs

# Can remove the word 'REGION' from the site data

dataset2 = dataset1
dataset2$site = str_sub(dataset1$site, start = 8)

# Check over

head(dataset2)
tail(dataset2)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'region_site_sitenumber_lat_long') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'No sites removed, but used subtring to remove text REGION which was present on all data points.  Had hard time searching for metadata, and what I call site number may not be an actual site number since there are some as egative values')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore
head(dataset2)
length(unique(dataset2$Species))

# Change name
names(dataset2)[4] = 'species'

# Check all levels over for bad species

levels(dataset2$species)

# Set to dataset3

dataset3 = dataset2

head(dataset3)

# No bad species to remove

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'No species removed, no changes')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Make countfield and rename field

names(dataset3)
countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# Check for NAs or zeros

summary(dataset3)
unique(dataset3$count)

# No  zeros or NAs found in count field

dataset4 = na.omit(dataset3)

# none in whole dataset
# Set to dataset5 to keep with template

dataset5 = dataset4

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat','Data represents abundance count. There were no NAs nor 0s that required removal')


#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# field representing sampling date?
datefield = 'Year'

# Date is just given in year
# Make sure it is numeric

class(dataset5$Year)

dataset6 = dataset5

dataset6$Year = as.numeric(dataset6$Year)

# Rename field

names(dataset6)[2] = 'date'

# Check

head(dataset6)
str(dataset6)
summary(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data given yearly. Only change made was to numeric')

# subannualTgrain.

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
