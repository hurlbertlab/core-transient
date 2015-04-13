# Formatting dataset 223: Sevilletta LTER Plants

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

ds = 223 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:

str(dataset)

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove them, note that I've given a new name here "dataset1", this is to ensure that we don't have to go back to square 1 if we've miscoded anything.

# If all fields will be used, then set unusedFields = 9999.

names(dataset)

unusedFields = c(1, 2, 8, 9, 11,13, 14)

dataset1 = dataset[,-unusedFields]

head(dataset1)

# Change name of date field

names(dataset1)[8] = 'date'

# Explore

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N') 

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Reminder of the dataset:

head(dataset1)

# We can see that sites are broken up into (potentially) 5 fields. Find the 
# metadata link in the data source table use that link to determine how
# sites are characterized.

# Metadata suggests the site data can be organized (largest to smallest) in this fashion: site > block > plot > treatment > quad

# Concatenate all of the potential fields to a site object

site = paste(dataset1$site, dataset1$block, dataset1$plot,  dataset$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site, 30)
tail(site, 30)

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset in case 
# you make a mistake:

dataset2 = dataset1

dataset2$site = factor(site)

head(dataset2)

dataset2 = dataset2[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit','site_block_treatment_plot_quad') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','Y')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'site fields concatenated. metadata suggests site-block-plot-quad describes the order of nested sites from small to large. The treatment field was not included because it does not represent a spatial unit within the blocks.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

levels(dataset2$species) 

# Uppercase to remove possible case error

dataset2$species = toupper(dataset2$species)

head(dataset2)

# Look through metadata for species codes. Found that names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

# reset factor levels and look for bad species

dataset2$species = factor(dataset2$species)

levels(dataset2$species)

bad_sp = c('','NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE','UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED', 'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

# Remove bad spp

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset the factor levels:

dataset3$species = factor(dataset3$species)

# Let's look at how the removal of bad species altered the length of the dataset:

nrow(dataset2)

nrow(dataset3)

# Look at the head of the dataset to ensure everything is correct:

head(dataset3)

summary(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat','several species removed. Metadata was relatively uninformative regarding what constitutes a true species sample for this study. Exploration of metadata from associated Sevilleta studies were more informative regarding which species needed to be removed. Species names are predominantly provided as Kartez codes, but not always. See: http://sev.lternet.edu/data/sev-212/5048. Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Original count field name

names(dataset3)
countfield = 'cover'

# Renaming it
names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Now we will remove zero counts and NA's:

summary(dataset3)

# Subset to records > 0:

dataset4 = subset(dataset3, count > 0) 

summary(dataset4)

# Remove NA's

dataset5 = na.omit(dataset4)

# How does it look?

head(dataset5)

length(unique(dataset5$count))
unique(dataset5$count)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat','cover')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents cover. Several NAs and some zeros were removed from original data.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# name of date field

datefield = 'date'

# Format of date field

dateformat = '%m/%d/%Y'

# Make date a date object

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# A check on the structure

class(date)

# Check and replace column

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field

dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Assign the new date values in a field called 'date'

dataset6$date = date

# Check the results:

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided as dates. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'Y')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# add the datasetID:

dataset1 = dataset

dataset1$datasetID = rep(223,nrow(dataset1))

# Change date to a factor:

dataset1$date = factor(as.character(dataset1$date))

# Now make the compiled dataframe:

dataset2 = ddply(dataset1,.(datasetID, site, date, species),
                 summarize, count = max(cover))

# Explore 

dim(dataset2)

head(dataset2)

summary(dataset2)

# Convert date back to a date object:

date = as.Date(dataset2$date, '%Y-%m-%d')

class(date)

head(date)

# All looks good, reassign the column:

dataset = dataset2

dataset$date = date
head(dataset)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset)

summary (dataset)

# write formatted data frame:

write.csv(dataset, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-
# COMMIT-PUSH THE UPDATED DATA FOLDER!


# cd data
# git add formatted_datasets/dataset_208.csv
# git commit -m "added formatted dataset"
# git push
# cd ..
# git add data
# git commit -m "updated submodule with formatted dataset 208"
# git push

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

# !!!At this point, go to the data source table and provide:
#   -central lat and lon (if available, if so, LatLonFLAG = 0, if you couldn't do
#    it, add a flag of 1)
#   -spatial_grain columns (T through W)
#   -nRecs, nSites, nTime, nSpecies
#   -temporal_grain columns (AH to AK)
#   -Start and end year
#   -Any necessary notes
#   -flag any issues and put issue on github
#   -git-add-commit-push data_source_table.csv

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$date))

length(unique(dataset$species))
