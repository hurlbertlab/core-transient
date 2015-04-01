# Formatting Dataset 147 OBIS Invert

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

ds = 147

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset, 10)
unique(dataset$SampleID)

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
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore

head(dataset1)
length(unique(dataset1$site))
summary(dataset1)

# Check all sites

levels(dataset1$site)

# sites are listed with a site name, number, and lat_long
# All sites begin with 'Pacific', so that part can be removed

dataset2 = dataset1
dataset2$site = str_sub(dataset2$site, start = 9)

head(dataset2)

# Looks good, sites now listed by number_lat_long

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit','sitename_sitenumber_lat_long') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N') 

# Notes_siteFormat. 

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites were all kept, but the name Pacific was removed from all sites so that sites are listed by number_lat_long')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore all the species

    # Rename field first
names(dataset2)[4] = 'species'

levels(dataset2$species) 

# Change to all caps

dataset2$species = toupper(dataset2$species)
length(unique(dataset1$Species))
length(unique(dataset2$species))

# No errors from letter case

# After looking through all species, no bad species detected, so no removals necessary

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'only change made was to uppercase all species.  no removals or other changes')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore
head(dataset2)

# Make countfield
countfield = "Abundance"

# Renaming it

dataset3 = dataset2
names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Remove zeros and NAs 

summary(dataset3$count)
unique(dataset3$count)

# none to remove, so no changes made

# Change to 5 to keep with template

dataset5 = dataset3

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', "count")

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'data represents species abundance at sites.  No changes were made')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# name of the field and format
datefield = 'Year'

dateformat = '%Y'

# Make date object

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# A check on the structure

class(date)

# numeric, so all good
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

# Notes_timeFormat

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided as years. only modification to this field involved converting to a numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Make final formatted dataframe
# DatasetID already in the dataset
# Now make the compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore the data frame:

dim(dataset7)

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!
#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

