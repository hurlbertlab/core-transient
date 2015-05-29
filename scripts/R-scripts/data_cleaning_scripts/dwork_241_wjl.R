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

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 241

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)

dim(dataset)

str(dataset)

head(dataset)

# Removed unneeded fields

# If all fields will be used, then set unusedFields = 9999.

names(dataset)

unusedFields = c(1,3)

dataset1 = dataset[,-unusedFields]

# Change "record_date" column to "date"

names(dataset1)[2] = 'date'

# Change "speciescode" column to "species"

names(dataset1)[4] = 'species'

head(dataset1, 10)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',   # Fill value in below
  
                                 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# According to the metadata, there are 5 swaths per station, with each swath being 2m x 10m. 

# Here, we will concatenate all of the potential fields that describe the site 
# in hierarchical order from largest to smallest grain:

site = paste(dataset1$station, dataset1$swath, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,c(2,4,5,6)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit',       # Fill value below in quotes
                                 
                                 'station_swath') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'Y') # Fill value here in quotes

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  # Fill value below in quotes
                                 
  'site fields concatenated. metadata suggests station_swath describes the order of nested sites from small to large.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

levels(dataset2$species) 

# Species entries are integers, change to factors:

dataset2$species = factor(dataset2$species)

# Look for possible errors or unidentifieds, compare codes with metadata

levels(dataset2$species)
table(dataset2$species)

# No problems

dataset3 = dataset2

# Check new length of dataset to make sure nothing has changed:

nrow(dataset2)

nrow(dataset3)

# Look at the head of the dataset to ensure everything is correct:

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat',    # Fill value below in quotes
                                 
  'No changes made to species column')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Next, we need to explore the count records. For filling out the data formatting table, we need to change the name of the field which represents counts, densities, percent cover, etc to "count". Then we will clean up unnecessary values.

names(dataset3)
summary(dataset3)

# Fill in the original field name here
countfield = 'density'

# Renaming it
names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Now we will remove zero counts and NA's:

summary(dataset3)

# Can usually tell if there are any zeros or NAs from that summary(). If there aren't any showing, still run these functions or continue with the update of dataset# so that you are consistent with this template.

# Subset to records > 0 (if applicable):

dataset4 = subset(dataset3, count > 0) 

summary(dataset4)

# Remove NA's:

dataset5 = na.omit(dataset4)


# How does it look?

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',    # Fill value below in quotes

                                 'density')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', # Fill value below in quotes

   'Data represents density. Many entries with value of 0 were removed')

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

# If the date is just a year, then make sure it is of class numeric
# and not a factor. Otherwise change to a true date object.

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# A check on the structure lets you know that date field is now a date object:

class(date)

# Give a double-check, if everything looks okay replace the column:

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field
dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Assign the new date values in a field called 'date'
dataset6$date = date

# Check the results:

head(dataset6)
str(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  # Fill value in below
                                 
                                 'temporal data provided as dates. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain',    # Fill value in below
                                 
                                 'Y')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for errors, and remove records that cant be used for our purposes.

# First, lets add the datasetID:

dataset6$datasetID = ds
  
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

# Update the data formatting table (this may take a moment to process). Note that the inputs for this are 'ds', the datasetID and the dataset form that you consider to be fully formatted.

dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, paste("data/formatted_datasets/dataset_", ds, ".csv", sep = ""), row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# As we've now successfully created the formatted dataset, we will now update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority',    # Fill value below in quotes 
                                 
                                 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag',    # Fill value below
                                 
                                 1)

# And update the data formatting table:

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))


