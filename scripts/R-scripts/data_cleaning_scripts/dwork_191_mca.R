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
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites were listed as North Atlantic regions and site numbers with other unknown information.  Region names were removed to simplify site data, but still plenty of unwanted info in the data.  Lat_long data is included at the end of each data string.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# First change the name of the field

names(dataset2)
names(dataset2)[4] = 'species'

# Explore

summary(dataset2)

# Make uppercase and look for bad species

dataset2$species = factor(toupper(dataset2$species))
levels(dataset2$species)

# No bad species found

dataset3 = dataset2

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no bad species, no changes made besides upper casing')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Name count field

names(dataset3)
countfield = "Abundance"

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# Check for NAs or zeros

summary(dataset3)
str(dataset3)
unique(dataset3$count)

# No zeros in count data

# Remove NAs if there are any

dataset4 = na.omit(dataset3)

# No NAs removed, change to dataset5 

dataset5 = dataset4

# Check for NAs or zeros

summary(dataset3)
str(dataset3)

# double check

head(dataset5, 20)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents count of abundance.There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

names(dataset5)
summary(dataset5)

# Data only given by year

datefield = 'Year'

# Data format

dateformat = "%Y"

# Make numeric object 

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# Check on the structure

class(date)

# Check over the dataset

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field
dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Add new field

dataset6$date = date

# Check

head(dataset6)
str(dataset6)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat','data provided as years. only modification to this field was converting to numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain','N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# Dataset ID number already in dataset

# Make compiled dataframe

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

