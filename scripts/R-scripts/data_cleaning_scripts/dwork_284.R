################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
#
# Dataset name: PISCO: Intertidal: Coastal Biodiversity Surveys: Quadrat Surveys.
# Dataset source (link): doi:10.6085/AA/pisco_intertidal.52.7 on DataOne
# Formatted by: Allen Hurlbert
#
# Start by opening the data formatting table (data_formatting_table.csv). 
# Datasets to be worked on will have a 'format_flag' of 0.

# Flag codes are as follows:
  # 0 = not currently worked on
  # 1 = formatting complete
  # 2 = formatting in process
  # 3 = formatting halted, issue
  # 4 = data unavailable
  # 5 = data insufficient for generating occupancy data

# NOTE: All changes to the data formatting table will be done in R! 
# Do not make changes directly to this table, this will create conflicting versions.

# YOU WILL NEED TO ENTER DATASET-SPECIFIC INFO IN EVERY LINE OF CODE PRECEDED
# BY "#--! PROVIDE INFO !--#". 

# YOU SHOULD RUN, BUT NOT OTHERWISE MODIFY, ALL OTHER LINES OF CODE.

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# This script is best viewed in RStudio. I like to reduced the size of my window
# to roughly the width of the section lines (as above). Additionally, ensure 
# that your global options are set to soft-wrap by selecting:
# Tools/Global Options .../Code Editing/Soft-wrap R source files

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)


# Source the functions file:

getwd()

# Set your working directory to be in the home of the core-transient repository
# e.g., setwd('C:/git/core-transient')

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('datasetID') you are working with.

#--! PROVIDE INFO !--#
datasetID = 284 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Make sure the original name of the raw data file is saved in the data formatting table.
# NOT, for example, 'rawdataset_255.csv', but the filename as originally downloaded.
# Check the data source link (available in the table, and hopefully posted above) if
# the data is available online. If the data come from a published paper and there is
# no file that was downloaded, enter "NA".

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
#--! PROVIDE INFO !--#
  'pisco_intertidal.43.2.csv') 



########################################################
# ANALYSIS CRITERIA                                    #  
########################################################

# Min number of time samples required 
minNTime = 6

# Min number of species required
minSpRich = 10

# Ultimately, the largest number of spatial and 
# temporal subsamples will be chosen to characterize
# an assemblage such that at least this fraction
# of site-years will be represented.
topFractionSites = 0.5

#######################################################

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset, and 
# determine what the fields represent and  which fields are relavent.

# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:


# View first 6 rows of the dataset:

head(dataset)


# Here, we can see that there are some fields that we won't use. These might be
# fields describing weather, observer ID's, or duplicate information like year
# or month when there is already a complete date column.

# 'section' describes whether a survey area was contiguous (1) or split into
# two sections (sections 1 and 2). Where they are split, the transect number
# does not appear to overlap between the two sections, so we can effectively
# ignore 'section'. 

# 'zone_code' is the numeric code that tells you in which zone the quadrat was 
# sampled. The high, mid, and low zones are categorized by the dominant species 
# in each zone, as well as the relative height above mean low low water (mllw).
# Could be useful, but potentially tricky, to do analyses across these zones, 
# but for now we ignore them, and all sites should include all/most zones.

# If all fields will be used, then set unusedFieldNames = ""

#--! PROVIDE INFO !--#
unusedFieldNames = c('survey_rep', 'section', 'sampler_code', 'zone_code')

dataset1 = dataset[, !names(dataset) %in% unusedFieldNames]

# Note that I've given a new name here "dataset1", this is to ensure that 
# we don't have to go back to square 1 if we've miscoded anything.

# Explore, if everything looks okay, you're ready to move forward. If not, 
# retrace your steps to look for and fix errors. 

head(dataset1, 10)

# I've found it helpful to explore more than just the first 6 data points given 
# with just a head(), so I used head(dataset#, 10) or even 20 to 50 to get a 
# better snapshot of what the data looks like.  Do this periodically throughout 
# the formatting process

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(datasetID, 'LatLong_sites',  

#--! PROVIDE INFO !--#
                                 'N') 


#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# What is the name of the field that has information on sampling date?
# If date info is in separate columns (e.g., 'day', 'month', and 'year' cols),
# then write these field names as a vector from largest to smallest temporal grain.
# E.g., c('year', 'month', 'day')

#--! PROVIDE INFO !--#
dateFieldName = c('year', 'month', 'day')

# If necessary, paste together date info from multiple columns into single field
if (length(dateFieldName) > 1) {
  newDateField = dataset1[, dateFieldName[1]]
  for (i in dateFieldName[2:length(dateFieldName)]) { newDateField = paste(newDateField, dataset[,i], sep = "-") }
  dataset1$date = newDateField
  datefield = 'date'
} else {
  datefield = dateFieldName
}

# What is the format in which date data is recorded? For example, if it is
# recorded as 5/30/94, then this would be '%m/%d/%y', while 1994-5-30 would
# be '%Y-%m-%d'. Type "?strptime" for other examples of date formatting.

#--! PROVIDE INFO !--#
dateformat = '%Y-%m-%d'

# If date is only listed in years:

# dateformat = '%Y'

# If the date is just a year, then make sure it is of class numeric
# and not a factor. Otherwise change to a true date object.

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset1[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset1[, datefield], dateformat))
}

# A check on the structure lets you know that date field is now a date object:

class(date)

# Give a double-check, if everything looks okay replace the column:

head(dataset1[, datefield])

head(date)

dataset2 = dataset1

# Delete the old date field
dataset2 = dataset2[, -which(names(dataset2) %in% dateFieldName)]

# Assign the new date values in a field called 'date'
dataset2$date = date

# Check the results:

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that 
# were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_timeFormat', 

#--! PROVIDE INFO !--#
    'Separate year, month and day fields merged')


# subannualTgrain. After exploring the time data, was this dataset sampled at a 
#   sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'subannualTgrain', 

#--! PROVIDE INFO !--#                                 
                                 'Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# From the previous head commmand, we can see that sites are broken up into 
# (potentially) 2 fields. Find the metadata link in the data formatting table use 
# that link to determine how sites are characterized.

#  -- If sampling is nested (e.g., quadrats within sites as in this study), use 
#     each of the identifying fields and separate each field with an underscore. 
#     For nested samples be sure the order of concatenated columns goes from 
#     coarser to finer scales (e.g. "km_m_cm")

# -- If sites are listed as lats and longs, use the finest available grain and 
#    separate lat and long fields with an underscore.

# -- If the site definition is clear, make a new site column as necessary.

# -- If the dataset is for just a single site, and there is no site column, then add one.

# Here, we will concatenate all of the potential fields that describe the site 
# in hierarchical order from largest to smallest grain. Based on the dataset,
# fill in the fields that specify nested spatial grains below.

#--! PROVIDE INFO !--#
site_grain_names = c("site_code", "transect", "location")

# We will now create the site field with these codes concatenated if there
# are multiple grain fields. Otherwise, site will just be the single grain field.
num_grains = length(site_grain_names)

site = dataset2[, site_grain_names[1]]
if (num_grains > 1) {
  for (i in 2:num_grains) {
    site = paste(site, dataset2[, site_grain_names[i]], sep = "_")
  } 
}

# What is the spatial grain of the finest sampling scale? 

dataFormattingTable[,'Raw_spatial_grain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain',  
                                 
#--! PROVIDE INFO !--#
                                 0.25) 

dataFormattingTable[,'Raw_spatial_grain_unit'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain_unit',  
                                 
#--! PROVIDE INFO !--#
                                 'm2') 


# BEFORE YOU CONTINUE. We need to make sure that there are at least minNTime for 
# sites at the coarsest possible spatial grain. 

siteCoarse = dataset2[, site_grain_names[1]]

if (dateformat == '%Y' | dateformat == '%y') {
  dateYear = dataset2$date
} else {
  dateYear = format(dataset2$date, '%Y')
}

datasetYearTest = data.frame(siteCoarse, dateYear)

ddply(datasetYearTest, .(siteCoarse), summarise, 
      lengthYears =  length(unique(dateYear)))

# If the dataset has less than minNTime years per site, do not continue processing. 

# Nope, no site here has been surveyed more than 3 times!



dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(datasetID, 'format_flag', 
     
#--! PROVIDE INFO !--#                                 
                                 5)

# Flag codes are as follows:
# 0 = not currently worked on
# 1 = formatting complete
# 2 = formatting in process
# 3 = formatting halted, issue
# 4 = data unavailable
# 5 = data insufficient for generating occupancy data

dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
  'No site has more than 3 temporal samples, although inclusion of post-2010 data might help in the future')


# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

