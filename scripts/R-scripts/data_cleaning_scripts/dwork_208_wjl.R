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

# Dataset number:

ds = 208 

# Get files

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

# Take out unneeded columns

unusedFields = c(6,7,9,10)

dataset1 = dataset[,-unusedFields]

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',   # Fill value in below
                                 
                                 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# I found a description of the spatial data collection available here:
# https://knb.ecoinformatics.org/#view/doi:10.5063/AA/mcolunga60.3.2

# My reading of this is that the entire study site was divided into 6 blocks
# ('Replicate', I think) and in each block there were 7 different 1-ha plots 
# each receiving a different cropping system treatment ('Treatment', which 
# in addition to 'T1' thru 'T7' also includes 'DF', 'CF', and 'SF' which I am 
# unclear on). Within each plot there are 5 permanent trap locations. 

# The spatial grain of analysis probably shouldn't be an individual sticky
# trap location. Possibly the grain could be the plot (a set of 5 trap locations)
# which would be the concatenation of Treatment and Replicate fields, or it 
# could be the entire block of 7 different treatments (i.e., just the Replicate
# field).

site = paste(dataset1$Replicate, dataset1$Treatment, sep = '_')

head(site)

# Replace site column

dataset2 = dataset1

dataset2$site = factor(site)

# Rearrange columns

dataset2 = dataset2[c(7,5,6,1)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit',       # Fill value below in quotes
                                 
                                 'treatment_replicate_station') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'Y') # Fill value here in quotes

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  # Fill value below in quotes
                                 
                                 'site fields concatenated. the metadata suggests that there are 6 replicates with 7 treatments each')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

names(dataset2)[2] = "species"
levels(dataset2$species) 

# The first thing that I notice is that there are lower and upper case entries. Because R is case-sensitive, this will be coded as separate species. Modify this prior to continuing:

dataset2$species = factor(toupper(dataset2$species))

# Now explore the listed species themselves, again. A good trick here to finding problematic entries is to shrink the console below horizontally so that species names will appear in a single column.  This way you can more easily scan the species names (listed alphabetically) and identify potential misspellings, extra characters or blank space, or other issues.

levels(dataset2$species)

# If species names are coded (not scientific names) go back to study's metadata to learn what species should and shouldn't be in the data. 

# In this example, a quick look at the metadata is not informative, unfortunately. Because of this, you should really stop here and post an issue on GitHub. With some more thorough digging, however, I've found the names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('SOMETHING ELSE')

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# It may be useful to count the number of times each name occurs, as misspellings or typos will likely
# only show up one time.

table(dataset3$species)

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
                                 
                                 'Unidentified entries (input as "something else") were removed')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. For filling out the data formatting table, we need to change the name of the field which represents counts, densities, percent cover, etc to "count". Then we will clean up unnecessary values.

names(dataset3)
summary(dataset3)

# Fill in the original field name here
countfield = 'Adults'

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
                                 
                                 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', # Fill value below in quotes
                                 
                                 'Data represents count of adults. Many 0s and some NAs were omitted.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# What is the name of the field that has information on sampling date?
datefield = 'Sample_Date'

# What is the format in which date data is recorded? For example, if it is
# recorded as 5/30/94, then this would be '%m/%d/%y', while 1994-5-30 would
# be '%Y-%m-%d'. Type "?strptime" for other examples of date formatting.

dateformat = '%Y-%m-%d'

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

dataset6$datasetID = 208

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

