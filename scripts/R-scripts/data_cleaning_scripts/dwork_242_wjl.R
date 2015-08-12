################################################################################*
#  Dataset 242 San Nicolas Island Benthic Fish
#
# Data and metadata can be found here: http://esapubs.org/archive/ecol/E094/244

# File "Benthic fish density raw data.csv", from fish surveys along transects
# 50 m long x 2 m wide (between the bottom and 2 m above bottom).

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

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('datasetID') you are working with.

datasetID = 242 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', datasetID, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

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

names(dataset)
dim(dataset)
str(dataset)
head(dataset)

# Remove unneeded fields

names(dataset)

unusedFields = c(1, 3)

dataset1 = dataset[,-unusedFields]

# Change 'record_date' to 'date':

names(dataset1)[2] = 'date'

names(dataset1)[4] = 'species'

head(dataset1, 10)

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(datasetID, 'LatLong_sites',   # Fill value in below
                                 
                                 'N') 


#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# What is the name of the field that has information on sampling date?
datefield = 'date'

# What is the format in which date data is recorded? For example, if it is
# recorded as 5/30/94, then this would be '%m/%d/%y', while 1994-5-30 would
# be '%Y-%m-%d'. Type "?strptime" for other examples of date formatting.

dateformat = '%m/%d/%y'

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
dataset2 = dataset2[, -which(names(dataset2) == datefield)]

# Assign the new date values in a field called 'date'
dataset2$date = date

# Check the results:

head(dataset2)
str(dataset2)

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_timeFormat',  # Fill value in below
                                 
                                 'temporal data provided as dates. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(datasetID, 'subannualTgrain',    # Fill value in below
                                 
                                 'Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# From the previous head commmand, we can see that sites are broken up into (potentially) 5 fields. Find the metadata link in the data formatting table use that link to determine how sites are characterized.

#  -- If sampling is nested (e.g., site, block, treatment, plot, quad as in this study), use each of the identifying fields and separate each field with an underscore. For nested samples be sure the order of concatenated columns goes from coarser to finer scales (e.g. "km_m_cm")

# -- If sites are listed as lats and longs, use the finest available grain and separate lat and long fields with an underscore.

# -- If the site definition is clear, make a new site column as necessary.

# -- If the dataset is for just a single site, and there is no site column, then add one.

# Here, we will concatenate all of the potential fields that describe the site 
# in hierarchical order from largest to smallest grain. Based on the dataset,
# fill in the fields that specify nested spatial grains below.

site_grain_names = c("station","transect")

# We will now create the site field with these codes concatenated if there
# are multiple grain fields. Otherwise, site will just be the single grain field.
num_grains = length(site_grain_names)

site = dataset2[, site_grain_names[1]]
if (num_grains > 1) {
  for (i in 2:num_grains) {
    site = paste(site, dataset2[, site_grain_names[i]], sep = "_")
  } 
}


# BEFORE YOU CONTINUE. We need to make sure that there are at least minNTime for sites at the coarsest possilbe spatial grain. 

siteCoarse = dataset2[, site_grain_names[1]]

if (dateformat == '%Y' | dateformat == '%y') {
  dateYear = dataset2$date
} else {
  dateYear = format(dataset2$date, '%Y')
}

datasetYearTest = data.frame(siteCoarse, dateYear)

ddply(datasetYearTest, .(siteCoarse), summarise, 
      lengthYears =  length(unique(dateYear)))

# All sites have at least 22 years, looks good

# Do some quality control by comparing the site fields in the dataset with the new vector of sites:

head(site)

# Check how evenly represented all of the sites are in the dataset. If this is the
# type of dataset where every site was sampled on a regular schedule, then you
# expect to see similar values here across sites. Sites that only show up a small
# percent of the time may reflect typos.

data.frame(table(site))

# All looks correct, so replace the site column in the dataset (as a factor) and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset3 = dataset2

dataset3$site = factor(site)

dataset3 = dataset3[,-c(1:2)]

# Check the new dataset (are the columns as they should be?):

head(dataset3)

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Raw_siteUnit',       
                                 
                                 'station_transect') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(datasetID, 'spatial_scale_variable',
                                 
                                 'Y') 

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_siteFormat',  
                                 
                                 'site fields concatenated. metadata suggests that there are 4-5 transects within each station.')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. For filling out the data formatting table, we need to change the name of the field which represents counts, densities, percent cover, etc to "count". Then we will clean up unnecessary values.

names(dataset3)
summary(dataset3)

# In case it is decided that juvenile density should not be used: 

# countfield = 'adultdensity'
# names(dataset2)[which(names(dataset2) == countfield)] = 'count'

# Otherwise, I will be adding the densities of adults and juveniles for each species

dataset3$count = dataset3$adultdensity + dataset3$juvdensity

head(dataset3)

dataset3 = dataset3[,-c(2,3)]

# The density field is calculated from 50 m × 2 m fish transects
# Let's replace it with a count of expected number of individuals per transect by 
# multiplying by 100.

dataset3$count = dataset3$count*100

# Now we will remove zero counts and NA's:

summary(dataset3)

# Subset to records > 0 (if applicable):

dataset4 = subset(dataset3, count > 0) 

summary(dataset4)

# Check to make sure that by removing 0's that you haven't completely removed
# any sampling events in which nothing was observed. Compare the number of 
# unique site-dates in dataset3 and dataset4.

# If there are no sampling events lost, then we can go ahead and use the 
# smaller dataset4 which could save some time in subsequent analyses.
# If there are sampling events lost, then we'll keep the 0's (use dataset3).
numEventsd3 = nrow(unique(dataset3[, c('site', 'date')]))
numEventsd4 = nrow(unique(dataset4[, c('site', 'date')]))
if(numEventsd3 > numEventsd4) {
  dataset4 = dataset3
} else {
  dataset4 = dataset4
}


# Remove NA's:

dataset5 = na.omit(dataset4)

head(dataset5)

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'countFormat',    # Fill value below in quotes
                                 
                                 'density')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_countFormat', # Fill value below in quotes
                                 
                                 'Data represents density. In the raw data, adult and juvenile densities were in separate columns. These were added together in count column.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

dataset5$species = factor(dataset5$species)

levels(dataset5$species)

# Removing entries of 'Sebastes spp.' 
# These could refer to any of the 9 species of Sebastes found in the study.
# There were other entries with unidentified species, but each had a unique genus, so there was no overlap like with Sebastes.
# Their species codes were 1031, 1032, 1034, 1038, 1040, 1052, 1060

bad_sp = c('1051')

dataset6 = dataset5[!dataset5$species %in% bad_sp,]

table(dataset6$species)

# Reset the factor levels:

dataset6$species = factor(dataset6$species)

# Let's look at how the removal of bad species and altered the length of the dataset:

nrow(dataset5)

nrow(dataset6)

# Look at the head of the dataset to ensure everything is correct:

head(dataset6)

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(datasetID, 'Notes_spFormat',    # Fill value below in quotes
                                 
                                 'Removed all entries of Sebastes spp. because there were multiple other entries of the Sebastes genus that were identified to the species. There were other entries with unidentified species, but each had a unique genus, so there was no overlap, so they were kept.  ')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for errors, and remove records that cant be used for our purposes.

# First, lets add the datasetID:

dataset6$datasetID = datasetID

# Now make the compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = sum(count))

# Explore the data frame:

dim(dataset7)

head(dataset7, 15)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!
#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table (this may take a moment to process). Note that the inputs for this are 'datasetID', the datasetID and the dataset form that you consider to be fully formatted.

dataFormattingTable = dataFormattingTableUpdate(datasetID, dataset7)

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, paste("data/formatted_datasets/dataset_", datasetID, ".csv", sep = ""), row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# As we've now successfully created the formatted dataset, we will now update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(datasetID, 'format_priority',    # Fill value below in quotes 
                                 
                                 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(datasetID, 'format_flag',    # Fill value below
                                 
                                 1)

# And update the data formatting table:

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

# Read in formatted dataset if skipping above formatting code (lines 1-450).

#dataset7 = read.csv(paste("data/formatted_datasets/dataset_",
#                         datasetID, ".csv", sep =''))

# Have a look at the dimensions of the dataset and number of sites:

dim(dataset7)
length(unique(dataset7$site))
length(unique(dataset7$date))
head(dataset7)

# Get the data formatting table for that dataset:

dataDescription = dataFormattingTable[dataFormattingTable$dataset_ID == datasetID,]

# or read it in from the saved data_formatting_table.csv if skipping lines 1-450.

#dataDescription = subset(read.csv("data_formatting_table.csv"),
#                             dataset_ID == datasetID)

# Check relevant table values:

dataDescription$LatLong_sites

dataDescription$spatial_scale_variable

dataDescription$Raw_siteUnit

dataDescription$subannualTgrain

# Before proceeding, we need to make decisions about the spatial and temporal grains at
# which we will conduct our analyses. Except in unusual circumstances, the temporal
# grain will almost always be 'year', but the spatial grain that best represents the
# scale of a "community" will  vary based on the sampling design and the taxonomic 
# group. Justify your spatial scale below with a comment.

tGrain = 'year'

# Refresh your memory about the spatial grain names

site_grain_names

sGrain = 'station'

# This is a reasonable choice of spatial grain because though there is a finer grained site value, the transect, it is too small (at 50m x 5m) to represent a community"

# The function "richnessYearSubsetFun" below will subset the data to sites with an 
# adequate number of years of sampling and species richness. If there are no 
# adequate years, the function will return a custom error message and you can
# try resetting sGrain above to something coarser. Keep trying until this
# runs without an error. If a particular sGrain value led to an error in this 
# function, you can make a note of that in the spatial grain justification comment
# above. If this function fails for ALL spatial grains, then this dataset will
# not be suitable for analysis and you can STOP HERE.

richnessYearsTest = richnessYearSubsetFun(dataset7, spatialGrain = sGrain, 
                                          temporalGrain = tGrain, 
                                          minNTime = minNTime, 
                                          minSpRich = minSpRich,
                                          dataDescription)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset7)
length(unique(richnessYearsTest$analysisSite))

# Temporary fix for the richnesYearSubsetFun issue where analysisdate gets listed as "year_year" instead of "year"

richnessYearsTest$analysisDate = substr(richnessYearsTest$analysisDate, start = 1, stop = 4)

# Once we've settled on spatial and temporal grains that pass our test above,
# we then need to 1) figure out what levels of spatial and temporal subsampling
# we should use to characterize that analysis grain, and 2) subset the
# formatted dataset down to that standardized level of subsampling.

# For example, if some sites had 20 spatial subsamples (e.g. quads) per year while
# others had only 16, or 10, we would identify the level of subsampling that 
# at least 'topFractionSites' of sites met (with a default of 50%). We would 
# discard "poorly subsampled" sites (based on this criterion) from further analysis. 
# For the "well-sampled" sites, the function below randomly samples the 
# appropriate number of subsamples for each year or site,
# and bases the characterization of the community in that site-year based on
# the aggregate of those standardized subsamples.

subsettedData = subsetDataFun(dataset7, datasetID, spatialGrain = sGrain, 
                              temporalGrain = tGrain,
                              minNTime = minNTime, minSpRich = minSpRich,
                              proportionalThreshold = topFractionSites,
                              dataDescription)

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))
