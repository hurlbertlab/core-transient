################################################################################*
#  DATASET 208 FORMATTING
################################################################################*

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

ds = 208 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

# Set the minimum number of time samples and number of species for analysis:

minNTime = 6

minSpRich = 10

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

names(dataset1)

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',   # Fill value in below
                                 
                                 'N') 


#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# What is the name of the field that has information on sampling date?
datefield = 'Sample_Date'

# What is the format in which date data is recorded? For example, if it is
# recorded as 5/30/94, then this would be '%m/%d/%y', while 1994-5-30 would
# be '%Y-%m-%d'. Type "?strptime" for other examples of date formatting.

dateformat = '%m/%d/%Y'

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
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  # Fill value in below
                                 
                                 'temporal data provided as dates. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain',    # Fill value in below
                                 
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

site = paste(dataset2$Replicate, dataset2$Treatment, sep = '_')

head(site)

# Replace site column

dataset3 = dataset2

dataset3$site = factor(site)

# Rearrange columns

dataset3 = dataset3[c(7,5,6,4)]

# Check the new dataset (are the columns as they should be?):

head(dataset3)


# BEFORE YOU CONTINUE. We need to make sure that there are at least minNTime for sites at the coarsest possilbe spatial grain. 

siteCourse = dataset3$site
dateYear = format(dataset3$date,'%Y')

datasetYearTest = data.frame(siteCourse, dateYear)

ddply(datasetYearTest, .(siteCourse), summarise, 
      lengthYears =  length(unique(dateYear)))

# At least 21 years for each site, looks good 

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

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',    # Fill value below in quotes
                                 
                                 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', # Fill value below in quotes
                                 
                                 'Data represents count of adults. Many 0s and some NAs were omitted.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

# Rename "Species" column to "species"

names(dataset5)[4] = 'species'

# Look at the individual species present:

levels(dataset5$species) 

# The first thing that I notice is that there are lower and upper case entries. Because R is case-sensitive, this will be coded as separate species. Modify this prior to continuing:

dataset5$species = factor(toupper(dataset5$species))

# Now explore the listed species themselves, again. A good trick here to finding problematic entries is to shrink the console below horizontally so that species names will appear in a single column.  This way you can more easily scan the species names (listed alphabetically) and identify potential misspellings, extra characters or blank space, or other issues.

levels(dataset5$species)

# If species names are coded (not scientific names) go back to study's metadata to learn what species should and shouldn't be in the data. 

# In this example, a quick look at the metadata is not informative, unfortunately. Because of this, you should really stop here and post an issue on GitHub. With some more thorough digging, however, I've found the names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('SOMETHING ELSE')

dataset6 = dataset5[!dataset5$species %in% bad_sp,]

# It may be useful to count the number of times each name occurs, as misspellings or typos will likely
# only show up one time.

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
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat',    # Fill value below in quotes
                                 
                                 'Unidentified entries (input as "something else") were removed')

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

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

datasetID = ds

# Get formatted dataset:

dataset = read.csv(paste("data/formatted_datasets/dataset_",
                         datasetID, ".csv", sep =''))

# Have a look at the dimensions of the dataset and number of sites:

dim(dataset)
length(unique(dataset$site))
length(unique(dataset$date))
head(dataset)

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check relevant table values:

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'location_web', 
                                          temporalGrain = 'season', 
                                          minNTime = minNTime, minSpRich = minSpRich)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 'location_web', temporalGrain = 'season',
                              minNTime = minNTime, minSpRich = minSpRich,
                              proportionalThreshold = .5)

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))
