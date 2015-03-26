Cleaning dataset 236:  Chile small mammals

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

ds = 236 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)

dim(dataset)

str(dataset)

head(dataset)

summary(dataset)

# Get raw info for formatting table

length(unique(dataset$mo))
length(unique(dataset$sp))

# Remove some unwanted fields
# Look through metadata to see which fields we don't need

# List fields to get rid of
unusedFields = c(1,3,4,6:9,11:16)

# New dataset without those fields

dataset1 = dataset[,-unusedFields]
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Site data given in grid numbers

dataset2 = dataset1

# change name

names(dataset2)[2] = 'site'
unique(dataset2$site)
class(dataset2$site)

# change to factor

dataset2$site = as.factor(dataset2$site)

# Check
head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 
dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', "grid number") 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',"N") 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  "checked metadata; sites are separated across 20 different 0.56 ha grids. no changes or removals were made.")

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore
head(dataset2)

levels(dataset2$sp)

# 2 species listed (MM and UN) not in metadata, UN probably means unidentified.  
# See how many MMs there are, if not many, probably a typo and can be eliminated

table(dataset2$sp)

# Only one 'MM' in whole dataset, so remove it and UN

bad_sp = c("MM","UN")

dataset3 = dataset2[!dataset2$sp %in% bad_sp,]

# Change field name and to factor

dataset3$sp = factor(dataset3$sp)

names(dataset3)[3] = 'species'

# Check results

nrow(dataset2)

nrow(dataset3)

head(dataset3)

levels(dataset3$species)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', "species coded, codes available in metadata. 2 species listed in dataset that were not present in metadata, so they were removed. Named MM and UN.  UN likely stands for unidentified")

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# No count in this dataset, but able to extract count because multiple records of each species at time samples and sites

dataset_count = data.frame(table(dataset3[,c('species','mo','site')]))

# check results

summary(dataset_count)
head(dataset_count, 100)

# Remove zeros

dataset5 = dataset_count[dataset_count$Freq!=0, ]

summary(dataset_count)

head(dataset_count)

# All worked, change name of field to count

names(dataset5)[4] = 'count'

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',  "NA")

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'no specific count data, but multiple records of same species at each site and single time samples. count values were created using table function.  Count field represents number of each species at each site per time sample.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# extract the sampling dates. 

datefield = 'mo'

# Change name to date

names(dataset5)[2] = 'date'

head(dataset5)

# Create separate date vector

date = dataset5$date

# Since date is formatted year-month with no space, need to substring out year and month and then paste back together

year = str_sub(date, end = 4)
month = str_sub(date, start = 5)

# No day in time data of this dataset. In order to make a date object, need to add 01 as a filler for the day

day = '01'

date = paste(month, day, year, sep = "/")

# Make date object

date1 = strptime(date,'%m/%d/%Y')
class(date1)
head(date1)

# add to dataset to replace old

dataset6 = dataset5

dataset6$date = date1
head(dataset6)

# Looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided in yearmonth format, no spaces. Used substring to separate month from year, then paste back together in correct format.  No days were provided, so needed to add a filler 01 as day to make into POSIX date object formmated m/d/y.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain',  'Y')

