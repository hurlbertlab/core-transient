# Formatting dataset 128: OBIS Zooplankton

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

ds = 128 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset, 100)
str(dataset)
summary(dataset)

# Remove column 'X'

dataset1 = dataset[,-1]

# Change name of ID to datasetID

names(dataset1)[1] = 'datasetID'

head(dataset1, 50)

# Change name of site field to site 

names(dataset1)[3] = 'site'

head(dataset1)

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# explore

length(unique(dataset1$site))

levels(dataset1$site)

# Sites are listed by USA_Atlantic_sitenumber
# Most sites are in intervals of 0.5, so round down to make whole numbers

site = as.character(dataset1$site)

# Make dataframe by separating string at underscores

siteTable = read.table(text = site, sep = "_")
head(siteTable)

# Make the site number column numeric

siteTable$V3 = as.numeric(siteTable$V3)
str(siteTable)

# Round the site numbers down to whole numbers

siteTable$V3 = floor(siteTable$V3)

# Check

summary(siteTable)
length(unique(siteTable$V3))
unique(siteTable$V3)

# All good, but there is a site listed as "0" so need to change back to character

siteTable$V3 = as.character(siteTable$V3)
summary(siteTable)
head(siteTable,30)

# Re-create site field

site1 = paste(siteTable$V1, siteTable$V2, siteTable$V3, sep = "_")

# Add new site field to dataset

dataset2 = dataset1
dataset2$site = site1

# Check site uniques

length(unique(site))
length(unique(site1))

# Check dataset with new sites

head(dataset2, 20)
tail(dataset2,20)
summary(dataset2)

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'ocean_sitenumber') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites are listed as USA_Atlantic_sitenumber.  Site numbers go by intervals of 0.5, so site units were separated and site numbers were rounded down to whole numbers.  Sites were then pasted back together and added back to dataset.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Change name of field

names(dataset2)[4] = 'species'

# Look at all individual species present

class(dataset2$species)
levels(dataset2$species) 

# Make uppercase

dataset2$species = factor(toupper(dataset2$species))
head(dataset2)

# Reset levels, search for bad species

levels(dataset2$species)

# No bad species found

dataset3 = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no bad species needed to be removed.  No changes to this field.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

names(dataset3)

# Name of countfield

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'

head(dataset3)

# REmove zeros and NAs

summary(dataset3)

# No zeros

dataset5 = na.omit(dataset3)

head(dataset5)

# No NAs removed, so no changes to field

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

# Name and format of the field

datefield = 'Year'
dateformat = '%Y'

# Make numeric object

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# Check 

class(date)

# Replace the column:

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field
dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Assign the new date values in a field called 'date'
dataset6$date = date

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided as years. only modification to this field involved converting to a numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# DatasetID already included

# Last look

head(dataset6, 20)

# Make compiled dataframe

dataset7 = ddply(dataset6, .(datasetID, site, date, species), summarize, 
                 count = max(count))

# Explore the data frame:

dim(dataset7)

head(dataset7, 30)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table (this may take a moment to process):

dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Final look at dataset

head(dataset7)

summary (dataset7)

# Write formatted data frame

write.csv(dataset7, "data/formatted_datasets/dataset_128.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority','NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag',  1)

# update the data formatting table

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))