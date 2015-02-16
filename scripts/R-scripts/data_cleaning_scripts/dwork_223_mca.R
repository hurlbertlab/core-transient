# Formatting dataset 223: Sevilletta LTER Plants

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:
getwd()
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_223.csv')


#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:

str(dataset)

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "d1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[,-c(1,2,8,11,13,14)]

head(dataset1)

# Because all (and only) the fields we want are present, we can re-assign d1:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# View summary of fields in the dataset:

summary(dataset)

# Reminder of the dataset:

head(dataset)

# We can see that sites are broken up into (potentially) 5 fields. Find the 
# metadata link in the data source table use that link to determine how
# sites are characterized.

# Concatenate all of the potential fields that describe the site

head(dataset)

site = paste(dataset$site, dataset$block, dataset$treatment, 
             dataset$plot, dataset$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset in case 
# you make a mistake:

dataset1 = dataset

dataset1$site = factor(site)

dataset1 = dataset1[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset1)

# All looks good, so overwrite the dataset file:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

sp = dataset$species

levels(sp)

# Uppercase to remove possible case error

dataset$species = toupper(dataset$species)

# Differences?

length(unique(dataset$species))

length(unique(sp))

# We see that almost 70 species were the result of upper and lower case!
# Make a new species vector (factor ensures that it is coded as a factor
# rather than character and removes any unused levels) 
# and continue exploring:

sp = factor(dataset$species)

levels(sp)

# Now explore the listed species themselves. To do so, you should go back to study's 
# metadata. A quick look at the metadata is not informative, unfortunately. Because of
# this, you should really stop here and post an issue on GitHub. With some more thorough
# digging, however, I've found the names represent "Kartez codes". Several species can
# be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta
# study (dataset 254) that provides species names for some codes). Some codes were identified
# with this pdf from White Sands: 
# https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('', 'NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE',
           'UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED',
           'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

dataset1 = dataset[!dataset$species %in% bad_sp,]

dataset1$species = factor(dataset1$species)

# Let's look at how the removal of bad species altered the length of the dataset:

nrow(dataset)

nrow(dataset1)

# Look at the head of the dataset to ensure everything is correct:

head(dataset1)

# Having checked through the results, we can now reassign the dataset:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# change the date column to a true date and replace name

head(dataset)

date = strptime(dataset$record_record_date, '%m/%d/%Y')

# A check on the structure lets you know that date field is now a date object:

class(dataset$record_record_date)

class(date)

# Give a double-check, if everything looks okay, then replace the column:

head(dataset$record_record_date)

head(date)

dataset1 = dataset

dataset1$record_record_date = date

names(dataset1)[5] = 'date'

# Let's remove the season field (for now):

dataset1 = dataset1[,-2]

# After a check of dataset1, you can rename it dataset:

head(dataset)

head(dataset1)

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# remove zero counts and NA's:

summary(dataset)

# Subset to records > 0 (if applicable):

dataset1 = subset(dataset, cover > 0) 

summary(dataset1)

# Remove NA's:

dataset1 = na.omit(dataset1)

# Make sure to write in the data summary table the type of observed count (here,
# it represents % cover)

# How does it look? If you approve,  assign changes to dataset:

summary(dataset)
summary(dataset1)

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# add the datasetID:

dataset1 = dataset

dataset1$datasetID = rep(223,nrow(dataset1))

# Change date to a factor:

dataset1$date = factor(as.character(dataset1$date))

# Now make the compiled dataframe:

dataset2 = ddply(dataset1,.(datasetID, site, date, species),
                 summarize, count = max(cover))

# Explore 

dim(dataset2)

head(dataset2)

summary(dataset2)

# Convert date back to a date object:

date = as.Date(dataset2$date, '%Y-%m-%d')

class(date)

head(date)

# All looks good, reassign the column:

dataset = dataset2

dataset$date = date
head(dataset)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset)

summary (dataset)

# write formatted data frame:

write.csv(dataset, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-
# COMMIT-PUSH THE UPDATED DATA FOLDER!


#################################################################################
# ---- END CREATION OF FORMATTED DATA FRAME ----
#################################################################################

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
# We have now formatted the dataset to the finest possible spatial and temporal
# grain, removed bad species, and added the dataset ID. It's now to make some
# scale decisions and determine the proportional occupancies.

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Because it's often considerably more straightforward, we'll start with the
# temporal data.

levels(dataset$site)

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? Return to the metadata to
# see if there's any clues.

levels(dataset$site)

#################################################################################
# ENDED CODING UPDATE HERE
#################################################################################
#######################################################################

# Our next task is to determine the temporal sampling grain. To do so
# we return to the online metadata. We see from the description of the 
# sampling design that quadrats were visited twice a year. 

# What was the temporal distribution of the two sampling periods?

months = months(date)

table(months)

# We can see that samples were collected in the spring and fall. We want
# these seasons. Of course we already knew that from the second column.

# Our next goal is to extract year from the date object:

year = as.numeric(format(date, '%Y'))

# We could pull seasonal data from months, but season is already 
# provided (albeit with year). Lets extract fall and spring from that
# field and change to numeric values of 0.25 (spring) and 0.75 (fall).

season = str_sub(dataset$season, end = -5)

season = ifelse(season == 'SPRING',.25, .75)

# Add year and season together and you have a date column provided in
# decimal years:

dataset$year = year + season 

# Check it out and if all looks okay, we'll remove the season and 
# date columns:

head(dataset)

# Okay we can see that the sampling is divided into fall and spring. Let's 
# just turn those into decimal years:

season = ifelse(season == 'SPRING',.25, .75)

summary(season)

# Now, let's extract year from the date (we could easily do this using
# the str_sub method as well, but this is more universal).
# First, make the date into an R date object:

class(dataset$record_record_date)

date = strptime(dataset$record_record_date, '%m/%d/%Y')

class(date)

head(date)

# It worked! Now extract year:

year = as.numeric(format(date, '%Y'))

# Add the decimal year and year vectors:

dataset$year = year + season 

head(dataset)

# Now let's clean up by removing the other date columns:

dataset1 = dataset[,-c(2,5)]

head(dataset1)

summary(dataset1)

# A couple of years are listed as NA. Let's remove them:

dataset1 = na.omit(dataset1)

summary(dataset1)

# Everything looks good, so let's call it d again

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!





#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# We can see that sites are broken up into (potentially) 5 fields. Find the 
# metadata link in the data source table use that link to determine how
# sites are characterized.
#  -- If sampling is nested (e.g., site, block, treatment, plot, quad as in 
# this study), use each of the identifying fields and separate each field with
# an underscore.
# -- If sites are listed as lats and longs, use the finest available grain 
# and separate lat and long fields with an underscore.
# -- If the site definition is clear, make a new site column as necessary.

# Here, we will concatenate all of the potential fields that describe the 
# site:

head(dataset)

site = paste(dataset$site, dataset$block, dataset$treatment, 
             dataset$plot, dataset$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset and remove the 
# unnecessary fields, start by renaming the dataset in case you make a mistake:

dataset1 = dataset

dataset1$site = site

dataset1 = dataset1[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset1)

# All looks good, so overwrite the dataset file:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# How many sites are there?

length(unique(dataset$site))

# How many records are there per site?

ddply(dataset, .(site), nrow)

# Hmmmm ... it seems the scale of site is off (and a conversation with
# Sevilleta confirmed this). What if we concatenated all of the site columns?
# Use paste to concatenate:

site = paste(dataset$site, dataset$block, dataset$treatment, 
             dataset$plot, dataset$quad, sep = '')

head(site)

length(unique(site))

# Now we have quite a few sites, how many records are there per site?
# Assign a name, because it's going to be super long:

siteTable = ddply(data.frame(site), .(site), nrow)

head(siteTable)

# Sort the table to see the fewest number of records per site:

head(siteTable[order(siteTable$V1),],10)

# Lot's of sites with few records! Let's explore further:

summary(siteTable)

# Let's try concatenating all but the quad field and explore the output:

site = paste(dataset$site, dataset$block, 
             dataset$treatment, dataset$plot, sep = '')

length(unique(site))

siteTable = ddply(data.frame(site), .(site), nrow)

head(siteTable[order(siteTable$V1),],10)

summary(siteTable)

# For all but the first site (and perhaps the second), these sample sizes are 
# adequate. Add to reduced dataframe:

dataset1 = dataset[,-c(2:5)]

dataset1$site = site

head(dataset1)

# Now let's remove the site with the very low sample size:

head(siteTable[order(siteTable$V1),],10)

dataset1 = dataset1[!dataset1$site %in% 'C3C1',]

head(dataset1)

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(dataset)

# Subset to records > 0

dataset1 = dataset[dataset$cover>0,]

summary(dataset1)

# Remove NA's:

dataset = na.omit(dataset1)

# Let's change the cover column to count. Make sure to write in the data summary
# table the type of observed count.

names(dataset)[4] = 'count'

head(dataset)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to modify the dates of sampling to decimal years. To do so, we 
# we need to be aware of the temporal grain of the analysis.

# Let's look at how the seasons are distributed by extracting just the 
# season (not year data):

head(dataset)

season = str_sub(dataset$season, end = -5)

levels(factor(season))

# Okay we can see that the sampling is divided into fall and spring. Let's 
# just turn those into decimal years:

season = ifelse(season == 'SPRING',.25, .75)

summary(season)

# Now, let's extract year from the date (we could easily do this using
# the str_sub method as well, but this is more universal).
# First, make the date into an R date object:

class(dataset$record_record_date)

date = strptime(dataset$record_record_date, '%m/%d/%Y')

class(date)

head(date)

# It worked! Now extract year:

year = as.numeric(format(date, '%Y'))

# Add the decimal year and year vectors:

dataset$year = year + season 

head(dataset)

# Now let's clean up by removing the other date columns:

dataset1 = dataset[,-c(2,5)]

head(dataset1)

summary(dataset1)

# A couple of years are listed as NA. Let's remove them:

dataset1 = na.omit(dataset1)

summary(dataset1)

# Everything looks good, so let's call it d again

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, lets add the datasetID:

dataset$datasetID = rep(223,nrow(dataset))

# Now make the data frame

dataset1 = ddply(dataset,.(datasetID, site, year, species), summarize, count = max(count))

# Give a quick look: 

head(dataset1)
dim(dataset1)
summary(dataset1)

# Now let's check and make sure each site has at least 10 species and 5 time
# samples:

siteTable = siteSummaryFun(dataset1)

head(siteTable)
dim(siteTable)
summary(siteTable)

# How many sites failed to pass the richness and time test?

badSites = badSiteFun(dataset1)

head(badSites)
dim(badSites)
summary(badSites)

# Remove bad sites

dataset1 = dataset1[!dataset1$site %in% badSiteFun(dataset)$site,]

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# If everything is looks okay (e.g., almost all, or at least most, sites have
# adequate), we're ready make and write formatted data frame:

dataset = dataset1

write.csv(dataset, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH BOTH YOUR COMPLETED SCRIPT AND THE NEW FORMATTED DATASET!

# And make our proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH propOcc!

# And make and write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_223.csv', row.names = F)

# Note: Both the submodule and core-transient folder need to be pushed to, 
# in git bash:

# cd data
# git add formatted_datasets/dataset_208.csv
# git commit -m "added formatted dataset"
# git push
# cd ..
# git add data
# git commit -m "updated submodule with formatted dataset 208"
# git push

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

# !!!At this point, go to the data source table and provide:
#   -central lat and lon (if available, if so, LatLonFLAG = 0, if you couldn't do
#    it, add a flag of 1)
#   -spatial_grain columns (T through W)
#   -nRecs, nSites, nTime, nSpecies
#   -temporal_grain columns (AH to AK)
#   -Start and end year
#   -Any necessary notes
#   -flag any issues and put issue on github
#   -git-add-commit-push data_source_table.csv

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$year))

length(unique(dataset$species))
