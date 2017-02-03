################################################################################*
#
# Dataset name:HJ Andrews TP108: Herb Layer in Uncut Plots: Cover, Height, Number of Tree Seedlings (U-B3)
# Dataset source (link):http://andrewsforest.oregonstate.edu/data/abstract.cfm?dbcode=TP108&topnav=97 
# Formatted by: Laura Hamon and Allen Hurlbert
#
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
datasetID = 290 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Make sure the original name of the raw data file is saved in the data formatting table.
# Check the data source link (available in the table, and hopefully posted above) if
# the data is available online. If the data come from a published paper and there is
# no file that was downloaded, enter "NA".

dataFormattingTable[,'Raw_datafile_name'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_datafile_name',  
                                 
#--! PROVIDE INFO !--#
  'TP10807_v3.csv') 



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

# Here, we can see that there are some fields that we won't use. Let's remove 
# them, note that I've given a new name here "dataset1", this is to ensure that 
# we don't have to go back to square 1 if we've miscoded anything.

# If all fields will be used, then set unusedFields = 9999.

names(dataset)

#--! PROVIDE INFO !--#
unusedFieldNames = c('DATACODE', 'ENTITY', 'DISTRICT', 'LIFEFORM','OCCURRENCE','HEIGHT1','TREESEED1','HEIGHT2','TREESEED2','HEIGHT3','TREESEED3','HEIGHT4','TREESEED4','HEIGHT5','TREESEED5','HEIGHT6','TREESEED6','PTYPE','PERSONNEL')


unusedFields = which(names(dataset) %in% unusedFieldNames)

dataset1 = dataset[,-unusedFields]

#change NA values to '0' and mean across all HCOVER columns 
hcoverFields = c('HCOVER1', 'HCOVER2', 'HCOVER3', 'HCOVER4','HCOVER5','HCOVER6')
dataset1$cover = apply(dataset1[, hcoverFields], 1, function(x) {x[is.na(x)] = 0; mean(x, na.rm=T)})

#remove HCOVER columns
unusedFields1 = which(names(dataset1) %in% hcoverFields)
dataset1 = dataset1[,-unusedFields1]


# You also might want to change the names of the identified species field [to 
# 'species'] and/or the identified site field [to 'site']. Just make sure you 
# make specific comments on what the field name was before you made the change, 
# as seen above.

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
dateFieldName = c('SAMPLEDATE')

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
dateformat = '%m/%d/%Y'

# If date is only listed in years:

# dateformat = '%Y'

# If the date is just a year, then make sure it is of class numeric
# and not a factor. Otherwise change to a true date object.

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset1[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset1[, datefield], dateformat))
}

# A check on the structure lets you know that date field is now a date object.
# Should say: [1] "POSIXct" "POSIXt"
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
    'The only modification to this field involved converting to a date object.')


# subannualTgrain. After exploring the time data, was this dataset sampled at a 
#   sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'subannualTgrain', 

#--! PROVIDE INFO !--#                                 
                                 'Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Good description of site layout and sampling design in Halpern et al. 2005 
# (Ecological Applications 15: 175-195) and 
# http://andrewsforest.oregonstate.edu/data/abstractdetail.cfm?dbcode=TP108&topnav=97

# Within 6 study locations ("BLOCK"), there are 5-8 treatments, and each treatment
# ("TRT") has a 8x8 or 7x9 grid containing (typically) 32 "PLOT"s. Each PLOT
# is made up of 4 TRANSects, each of which consists of six 0.2 x 0.5 m microplots, 
# so the scale of a TRANSect is 0.6 m2, of a PLOT is 2.4 m2, and of a TRT is 76.8 m2.

# Here, we will concatenate all of the potential fields that describe the site 
# in hierarchical order from largest to smallest grain. Based on the dataset,
# fill in the fields that specify nested spatial grains below.

#--! PROVIDE INFO !--#
site_grain_names = c("BLOCK", "TRT", "PLOT", "TRANS")

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
                                 0.6) 

dataFormattingTable[,'Raw_spatial_grain_unit'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_spatial_grain',  
                                 
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

#  siteCoarse lengthYears
#1          1           3
#2          4           3
#3          5           3
#4          6           3
#5          7           3
#6          8           1


# NOPE! Although there are 6 years of data in the dataset, no site was surveyed
# for more than 3 years

###################################################################################
#
# STOP PROCESSING AND SET FORMAT FLAG TO 5

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

# Add any final notes about the dataset that might be of interest:
dataFormattingTable[,'General_notes'] = 
  dataFormattingTableFieldUpdate(datasetID, 'General_notes', 
                                 
                                 #--! PROVIDE INFO !--#                                 
  'Although there are 6 years of data, no site was surveyed for more than 3 years.')



# And write the final data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

