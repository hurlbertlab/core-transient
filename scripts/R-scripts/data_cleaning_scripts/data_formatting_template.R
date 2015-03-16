################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
# Start by opening the data formatting table (data_formatting_table.csv). To determine which dataset you should be working on, see the "format_priority" field. Choose the dataset with the highest format priority, but be sure to check out the format_flag field to see the current status of the dataset.

# Flag codes are as follows:
  # 0 = not currently worked on
  # 1 = formatting complete
  # 2 = formatting in process
  # 3 = formatting halted, issue
  # 4 = data unavailable

# NOTE: All changes to the data formatting table will be done in R! Do not make changes directly to this table, this will create conflicting versions.

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

# Get data. First specify the dataset number ('ds') you are working with.

ds = 223 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset, and determine what the fields represent and  which fields are relavent.

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

# Let's change the name of the "record_record_date" column to simply "date":

names(dataset1)[8] = 'date'

# Explore, if everything looks okay, you're ready to move forward. If not, retrace your steps to look for and fix errors. 

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',   # Fill value in below
  
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
# in hierarchical order from largest to smallest grain:

site = paste(dataset1$site, dataset1$block, dataset1$treatment, 
             dataset1$plot, dataset1$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit',       # Fill value below in quotes
                                 
                                 'site_block_treatment_plot_quad') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'Y') # Fill value here in quotes

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  # Fill value below in quotes
                                 
  'site fields concatenated. metadata suggests site-block-treatment-plot-quad describes the order of nested sites from small to large.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

levels(dataset2$species) 

# The first thing that I notice is that there are lower and upper case entries. Because R is case-sensitive, this will be coded as separate species. Modify this prior to continuing:

dataset2$species = toupper(dataset2$species)

# Now explore the listed species themselves. To do so, you should go back to study's metadata. A quick look at the metadata is not informative, unfortunately. Because of this, you should really stop here and post an issue on GitHub. With some more thorough digging, however, I've found the names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('','NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE','UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED', 'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset the factor levels:

dataset3$species = factor(dataset3$species)

# Let's look at how the removal of bad species and altered the length of the dataset:

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
                                 
  'several species removed. Metadata was relatively uninformative regarding what constitutes a true species sample for this study. Exploration of metadata from associated Sevilleta studies were more informative regarding which species needed to be removed. Species names are predominantly provided as Kartez codes, but not always. See: http://sev.lternet.edu/data/sev-212/5048. Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. For filling out the data formatting table, we need to change the name of the field which represents counts, densities, percent cover, etc to "count". Then we will clean up unnecessary values.

names(dataset3)

# Fill in the original field name here
countfield = 'density'

# Renaming it
names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Now we will remove zero counts and NA's:

summary(dataset3)

# Subset to records > 0 (if applicable):

dataset4 = subset(dataset3, count > 0) 

summary(dataset4)

# Remove NA's:

dataset5 = na.omit(dataset4)


# How does it look?

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are ... **************#$@$*#*#$*#$*
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',    # Fill value below in quotes

                                 'cover')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', # Fill value below in quotes

   'Data represents cover. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# What is the name of the field that has information on sampling date?
datefield = 'date'

# What is the format in which date data is recorded? For example, if it is
# recorded as 5/30/94, then this would be '%m/%d/%y', while 1994-5-30 would
# be '%Y-%m-%d'. Type "?strptime" for other examples of date formatting.

dateformat = '%m/%d/%Y'

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
dataset6 = dataset6[, -which(names(dataset6) == 'datefield')]

# Assign the new date values in a field called 'date'
dataset6$date = date

# Check the results:

head(dataset6)

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
                 summarize, count = max(count))

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

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_ds.csv", row.names = F)

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


