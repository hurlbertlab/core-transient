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
setwd('C:/Users/auriemma/core-transient/')
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
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'Y') 

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

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', "lat_long") 


# spatial_scale_variable

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N')


# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites were listed as South_Africa_lat_long, so I used substring to remove the South Africa text, leaving only lat_long data. No changes or removals were made to the actual data.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Look at the individual species present:

levels(dataset2$Species) 

# All species are good

length(unique(dataset2$Species))

# 68 unique species

# Change name to 'species'

dataset3 = dataset2

names(dataset3)[4] = 'species'

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no changes made')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore

head(dataset3)
summary(dataset3)
unique(dataset3$Abundance)
class(dataset3$Abundance)

# Fill in the original field name here
countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# No zeros or NAs in data, so no need to remove any count values

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'Abundance')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', "Data represents abundance. no changes or removals necessary")

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# What is the name of the field that has information on sampling date?

datefield = 'Year'

# What format?
dateformat = '%Y'

# Date is in just year
dataset5 = dataset3

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# check

class(date)

# Worked, so add the new date to dataset and remove old

dataset6 = dataset5
dataset6$date = date
dataset6 = dataset6[,-2]

# check results

head(dataset6)
summary(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. 

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided yearly. only change made was to a numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# DatasetID already in dataset, so just change name

names(dataset6)[1] = 'datasetID'

# Last check over

head(dataset6)
str(dataset6)

# Make summary dataset

dataset7 = ddply(dataset6,.(datasetID, site, date, species), summarize, 
                 count = max(count))

# Explore the data frame:

dim(dataset7)

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table (this may take a moment to process):

dataFormattingTable = dataFormattingTableUpdate(ds)

# final look at the dataset:
  
head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_67.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# update the format priority and format flag fields

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority','NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag', 1)

# And update the data formatting table:

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)



# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))