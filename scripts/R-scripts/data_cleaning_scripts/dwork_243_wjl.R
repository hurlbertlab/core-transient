################################################################################*
#  Dataset 243: San Nicolas Island Midwater Fish
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

getwd()

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 243

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

# Remove unneeded fields

unusedFields = c(1,3)

dataset1 = dataset[,-unusedFields]

# Let's change the name of the "record_date" column to simply "date":

names(dataset1)

names(dataset1)[2] = 'date'

names(dataset1)[4] = 'species'

# You also might want to change the names of the identified species field [to 'species'] and/or the identified site field [to 'site']. Just make sure you make specific comments on what the field name was before you made the change, as seen above.

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

# Concatenate site fields

site = paste(dataset1$station, dataset1$transect, sep = '_')

head(site)

# Check how evenly represented all of the sites are in the dataset. If this is the
# type of dataset where every site was sampled on a regular schedule, then you
# expect to see similar values here across sites. Sites that only show up a small
# percent of the time may reflect typos.

data.frame(table(site))

# All looks correct, so replace the site column in the dataset (as a factor) and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,-c(1,3)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit',       # Fill value below in quotes
                                 
                                 'station_transect') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable',
                                 
                                 'Y') # Fill value here in quotes

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  # Fill value below in quotes
                                 
                                 'site fields concatenated. metadata suggests that there are 4-5 transects within each station.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

names(dataset2)
summary(dataset2)

# In case it is decided that juvenile density should not be used: 

# countfield = 'adultdensity'
# names(dataset2)[which(names(dataset2) == countfield)] = 'count'

# Otherwise, I will be adding the densities of adults and juveniles for each species

dataset2$count = dataset2$adultdensity + dataset2$juvdensity

dataset2 = dataset2[,-c(3,4)]

# Now we will remove zero counts and NA's:

summary(dataset2)

# Subset to records > 0 (if applicable):

dataset3 = subset(dataset2, count > 0) 

summary(dataset3)

# Remove NA's:

dataset4 = na.omit(dataset3)

# How does it look?

head(dataset4)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',    # Fill value below in quotes
                                 
                                 'cover')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', # Fill value below in quotes
                                 
                                 'Data represents cover. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

levels(dataset4$species) 

# The first thing that I notice is that there are lower and upper case entries. Because R is case-sensitive, this will be coded as separate species. Modify this prior to continuing:

dataset4$species = factor(toupper(dataset4$species))

# Now explore the listed species themselves, again. A good trick here to finding problematic entries is to shrink the console below horizontally so that species names will appear in a single column.  This way you can more easily scan the species names (listed alphabetically) and identify potential misspellings, extra characters or blank space, or other issues.

levels(dataset4$species)

# If species names are coded (not scientific names) go back to study's metadata to learn what species should and shouldn't be in the data. 

# In this example, a quick look at the metadata is not informative, unfortunately. Because of this, you should really stop here and post an issue on GitHub. With some more thorough digging, however, I've found the names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('','NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE','UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED', 'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

dataset5 = dataset4[!dataset4$species %in% bad_sp,]

# It may be useful to count the number of times each name occurs, as misspellings or typos will likely
# only show up one time.

table(dataset5$species)

# If you find any potential typos, try to confirm that the "mispelling" isn't actually a valid name.
# If not, then go ahead and replace all instances like this:

typo_name = ''
good_name = ''

dataset5$species[dataset$species == typo_name] = good_name


# Reset the factor levels:

dataset5$species = factor(dataset5$species)

# Let's look at how the removal of bad species and altered the length of the dataset:

nrow(dataset4)

nrow(dataset5)

# Look at the head of the dataset to ensure everything is correct:

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat',    # Fill value below in quotes
                                 
                                 'several species removed. Metadata was relatively uninformative regarding what constitutes a true species sample for this study. Exploration of metadata from associated Sevilleta studies were more informative regarding which species needed to be removed. Species names are predominantly provided as Kartez codes, but not always. See: http://sev.lternet.edu/data/sev-212/5048. Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf')

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

# If date is only listed in years:

dateformat = '%Y'

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

write.csv(dataFormattingTable, 'data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))


