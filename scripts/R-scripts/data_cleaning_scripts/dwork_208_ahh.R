################################################################################*
#  DATA CLEANING TEMPLATE
################################################################################*

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_208.csv')

#===============================================================================*
# MAKE FORMATTED DATASET
#===============================================================================*
# The goal is: 
# 1) To create a dataset of the columns of interest at the smallest spatial
#  and temporal sampling grain available.
# 2) To eliminate "bad" species data.

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset,
# what the fields represent and determining which fields are relavent. Do 
# this carefully, making notes on a piece of scratch paper.

# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:

str(dataset)

#'data.frame':  48443 obs. of  9 variables:
#$ Sample_Date      : Factor w/ 765 levels "1989-05-24","1989-06-07",..: 765 765 765 765 765 765 765 765 765 765 ...
#$ Treatment        : Factor w/ 10 levels "CF","DF","SF",..: 2 10 10 10 2 10 2 10 10 3 ...
#$ Replicate_Station: Factor w/ 30 levels "1_1","1_2","1_3",..: 13 1 26 26 14 11 15 11 11 1 ...
#$ Species          : Factor w/ 22 levels "Adalia bipunctata",..: 18 3 10 9 18 10 18 3 6 18 ...
#$ Family           : Factor w/ 8 levels "","Cantharidae",..: 7 2 4 4 7 4 7 2 4 7 ...
#$ Order            : Factor w/ 5 levels "","Coleoptera",..: 4 2 2 2 4 2 4 2 2 4 ...
#$ Adults           : int  2 1 2 1 4 1 6 1 1 1 ...
#$ location_utm     : Factor w/ 255 levels "(631819.64,4697018.97)",..: 38 85 42 42 39 170 40 170 170 17 ...
#$ Year             : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "d1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[, !names(dataset) %in% c("Family", "Order", "location_utm")]

head(dataset1)

# Rename fields using standardized names where appropriate
names(dataset1)[names(dataset1) %in% c('Species', 'Adults', 'Year')] = c('species', 'count', 'date')

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

# I found a description of the spatial data collection available here:
# https://knb.ecoinformatics.org/#view/doi:10.5063/AA/mcolunga60.3.2

# My reading of this is that the entire study site was divided into 6 blocks
# and in each block there were 7 different 1-ha plots each receiving a different
# cropping system treatment ('Treatment', which in addition to 'T1' thru 'T7'
# also includes 'DF', 'CF', and 'SF' which I am unclear on). Within each plot 
# there are 5 permanent trap locations. 


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

site = paste(dataset$Treatment, dataset$Replicate_Station, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# Are these site names equitably represented in the dataset?

hist(table(site))

# For dataset 208, the majority of sites have between 150-250 rows of data 
# associated with them, with a nice narrow normal distribution. Seems good.

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset in case 
# you make a mistake:

dataset1 = dataset

dataset1$site = factor(site)

dataset1 = dataset1[, !names(dataset1) %in% c("Treatment", "Replicate_Station")]

# Check the new dataset (are the columns as they should be?):

head(dataset1)

# All looks good, so overwrite the dataset file:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do
# so, you need to look at the list of unique species very carefully. Avoid being
# too liberal in interpretation, if you notice an entry that MIGHT be a problem, 
# but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

sp = dataset$species

levels(sp) # Note: You can also use unique(sp) here.

# Now explore the listed species themselves. To do so, you should go back to study's 
# metadata. 

bad_sp = c('something else')

dataset1 = dataset[!dataset$species %in% bad_sp,]

dataset1$species = factor(dataset1$species)

# Let's look at how the removal of bad species altered the length of the dataset:

nrow(dataset) #48443

nrow(dataset1) #47854

# Look at the head of the dataset to ensure everything is correct:

head(dataset1)

# Having checked through the results, we can now reassign the dataset:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# For starters, let's make sure that the number of sampling events per site
# within a year does not change systematically across years.

head(dataset)
uniqSiteDate = unique(dataset[, c('Sample_Date', 'date', 'site')])
samplingPerSiteYear = data.frame(table(uniqSiteDate[, c('date', 'site')]))
samplingPerSiteYear = samplingPerSiteYear[samplingPerSiteYear$Freq != 0, ]
meanSamplingPerYear = aggregate(samplingPerSiteYear$Freq, 
                                by = list(samplingPerSiteYear$date), mean)
plot(as.numeric(as.character(samplingPerSiteYear$date)), 
    samplingPerSiteYear$Freq, xlab = 'Year', 
    ylab = 'Mean sampling events per site')
points(as.numeric(as.character(meanSamplingPerYear$Group.1)),
       meanSamplingPerYear$x, type = 'l', lwd = 3)

# There is some interannual variation in sampling intensity, and a weak trend
# in recent years towards more sampling events per year.


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
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

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
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, lets add the datasetID:

dataset1 = dataset

dataset1$datasetID = rep(223,nrow(dataset1))

# Change date to a factor:

dataset1$date = factor(as.character(dataset1$date))

# Now make the compiled dataframe:

dataset2 = ddply(dataset1,.(datasetID, site, date, species),
                 summarize, count = max(cover))


# Explore the data frame:

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

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset)

summary (dataset)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-
# COMMIT-PUSH THE UPDATED DATA FOLDER!


################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*
# If you had to set the formatting script aside and opened this in a new 
# session, source the script and load required libraries and dataset:

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_223.csv")

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
# temporal data. For this, go to the metadata of the study. There, we see
# that data are collected in two seasons, spring and fall. We want to change
# these to a decimal year, which are simply grouped time samples (the actual 
# values don't really matter much).

# Extract year values:

year = as.numeric(format(dataset$date, '%Y'))

head(year)

summary(year)

# Extract month values (if applicable):

month = as.numeric(format(dataset$date, '%m'))

head(month)

summary(month)

unique(month)

# Make "season" values:

season = ifelse(month < 7, .25, .75)

unique(season)

# Add year and season:

yearSeason = year + season

head(yearSeason)

summary(yearSeason)

# Modify date column to now represent the decimal year:

dataset1 = dataset

dataset1$date = yearSeason

head(dataset1)

# Change the column name to year:

names(dataset1)

names(dataset1)[3] = 'year'

# Summarize the dataset to the new temporal grain:

dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

# Explore:

dim(dataset1)
dim(dataset2)

head(dataset1)
head(dataset2)

str(dataset2)

# All looks okay, rename as dataset:

dataset = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY TEMPORAL GRAIN DECISIONS!

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? Return to the metadata to
# see if there's any clues.

# How many sites are there?

length(unique(dataset$site))

# How many time and species records are there per site?

siteTable = ddply(dataset, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

# We see that each of the sites was sampled with equivalent, and adequate,
# time samples (>4) but that at least some sites have species richness 
# below the cut-off value of 10. Perhaps too many sites of with low sr?

# Let's sort and have a look at the first few rows:

head(siteTable[order(siteTable$nSp),],20)

# All 1's! How many sites have less than 10 species?

nrow(siteTable)
nrow(subset(siteTable, nSp < 10))

# That's almost a third of the sites! This is a clue that the 
# smallest spatial sampling grain (quadrat) is too fine.

# Let's try concatenating all but the quad field and explore the output. 
# We start by splitting site:

site = read.table(text = as.character(dataset$site), sep ='_')

head(site)

site1 = do.call('paste', c(site[,1:4],sep = '_'))

head(site1)

length(site1)

# How have we changed the number of sites?

length(unique(dataset$site))

length(unique(site1))

# We've reduced the number of sites to 28! How does the richness look
# for this new spatial sampling grain?

dataset1 = dataset

dataset1$site = site1

siteTable = ddply(dataset1, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

head(siteTable[order(siteTable$nSp),],10)

# For all but the first site (and perhaps the second), the species richness
# is adequate.Change the dataset site column to this one:

dataset$site = dataset1$site

# Now let's remove the sites with inadequate sample sites:

badSites = subset(siteSummaryFun(dataset), spRich < 10 | nTime < 5)$site

dataset1 = dataset[!dataset$site %in% badSites,]

# Summarize the dataset to the new spatial grain:

dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

head(dataset2)

dim(dataset2)

summary(dataset2)

# All looks good, rename dataset:

dataset = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY SPATIAL GRAIN DECISIONS!

# Note: In many instances, site definition will be spatially explicit (e.g., 
# lats and longs). When this is the case, we may need to summarize the data to
# a courser precision (few decimal places). We can do so by using the 
# "round_any" function in Hadley Wickham's plyr package, specifying "floor" 
# as the rounding function.

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

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

