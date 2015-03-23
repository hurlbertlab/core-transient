# Cleaning dataset 173 OBIS Marine Inverts

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

# Get data

ds = 173 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Explore

dim(dataset)
names(dataset)
str(dataset)
head(dataset)
summary(dataset)

# Remove unwanted columns
  # List removed fields

unusedFields = c(1,2)
dataset1 = dataset[,-unusedFields]

# Check

dim(dataset1)
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Make site column

dataset2 = dataset1

# just renamed only site column from "sampleID" to "site"

names(dataset2)[2] = "site"
head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit',       # Fill value below in quotes
                                 
                                 'site')

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'N')

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'no changes were made to raw data. sites are listed by name, and some have different number IDs.')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Look at each individual species to find ones to remove

dataset2 = dataset1
levels(dataset2$Species)

# No questionable or bad species

# Change name of field

head(dataset2)
names(dataset2)[3] = "species"

# Check
names(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no changes made to species column.  Wide variety of taxonomic resolution, but there are no unidentified or bad species in data.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore

dataset3 = dataset2

# Make count column

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Remove zeros and NAs

summary(dataset3)

# Subset to records > 0:

dataset4 = subset(dataset3, count > 0) 
summary(dataset4)

# Remove NA's:

dataset5 = na.omit(dataset4)

# Check over 

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents count, originally labeled as abundance. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# extract the sampling dates. 

# name of the field
datefield = 'Year'

# What is the format in which date data is recorded?
class(dataset5$Year)

# If the date is just a year, then make sure it is of class numeric
# and not a factor. Otherwise change to a true date object.

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# Check
class(date)

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field

dataset6 = dataset6[, -which(names(dataset6) == datefield)]
head(dataset6)

# Add new date field

dataset6$date = date

# Check the results:

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  'temporal data provided as year of sample. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Add the datasetID:

dataset6$datasetID = ds
head(dataset6)

# Now make the compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# final look at the dataset:

head(d)
summary(d)

# looks okay, write formatted data frame

write.csv(d, "data/formatted_datasets/dataset_173.csv", row.names = F)

################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*
library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

d = read.csv("data/formatted_datasets/dataset_173.csv")

head(d)

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Year is default temporal grain size

# change date column to factor
d$date = factor(as.character(d$date))
class(d$date)

# Temporal grain for this dataset is already year

# Change column name:

names(d)[3] = 'year'
head(d, 30)

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*

# How many sites are there?
length(unique(d$site))
  # 758 unique sites, so likely several with low number of records

# How many species and time samples per site
siteTable = ddply(d, .(site), summarize, nyear= length(unique(year)), 
                  nsp = length(unique(species)))
head(siteTable, 20)
tail(siteTable)
summary(siteTable)

# All sites have only 1 time sample and several have less than 10 spp
# Need to expand the scope of each site 
# Look at all sites to find consistencies across all data

unique(d$site)

# All sites are listed by given name, and most followed by numeric 
# (2-4 digits) site name as well. All words and numeric indicators are
# separated by underscores.

# After looking through all site names, the ones that appear several times
# are the ones that have different numeric indicators at the end of the name.
# So a good way to expand the scope would be to substring the last 4 characters
# of the sites to remove numbers and get just the names.

site1 = str_sub(site, end = -4)
head(site1, 30)
head(site, 30)
tail(site1, 30)
# There were 2 spaces after the numbers in sites, so remove 2 more characters
site1 = str_sub(site, end = -6)
head(site1, 50)

# it worked, now plug new sites into dataset
d1 = d
d1$site = site1
head(d1, 20)

# Now check siteTable for year and species sample sizes per site
siteTable = ddply(d1, .(site), summarize, nyear = length(unique(year)),
                                          nsp = length(unique(species)))
head(siteTable, 20)
head(siteTable[order(siteTable$nyear),], 30)
tail(siteTable[order(siteTable$nyear),], 30)
length(unique(site1))

