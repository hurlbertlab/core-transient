################################################################################*
# EXAMPLE DATA CLEANING SCRIPT
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

dataset = read.csv('data/raw_datasets/dataset_250.csv')

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

# View first 6 rows of the dataset:

head(dataset)

# View the first 10 rows of the dataset:

head(dataset, 10)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "d1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[,-c(1,3,4,7:9)]

head(dataset1)

# Because all (and only) the fields we want are present, we can re-assign d1:

dataset = dataset1

# View summary of fields in the dataset:

summary(dataset)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Reminder of the dataset:

head(dataset)

# How many sites are there?

length(unique(dataset$creek_location))

# How many records are there per site?

ddply(dataset, .(creek_location), nrow)

# Sites look fine, moving on to species.

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do
# so, you need to look at the list of unique species very carefully. Avoid being
# too liberal in interpretation, if you notice an entry that MIGHT be a problem, 
# but you can't say with certainty, create an issue on GitHub.

sp = dataset$fish_species

levels(sp) # Note: You can also use unique(sp) here.

# All looks fine, moving on to counts.

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(dataset)

head(dataset)

# In this case, each record is an individual. We want to summarize to count the
# number of individuals at each site for a given species.

dataset1 = ddply(dataset, .(creek_location,year, fish_species),summarize,
                 count = length(fish_species))

summary(dataset1)

head(dataset1)

# All looks good, renaming to dataset:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Time samples, at an annual grain, are fine for our needs. This study can,
# however, be used to look at changing temporal grain.

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, lets add the datasetID:

dataset$datasetID = rep(250,nrow(dataset))

# Now make the data frame

head(dataset)

dataset1 = dataset[,c(5,1:4)]

names(dataset1) = c('datasetID', 'site','year','species','count')

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

dataset1 = dataset1[!dataset1$site %in% badSiteFun(dataset1)$site,]

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# If everything is looks okay (e.g., almost all, or at least most, sites have
# adequate), we're ready make and write formatted data frame:

dataset = dataset1

write.csv(dataset, "data/formatted_datasets/dataset_250.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH BOTH YOUR COMPLETED SCRIPT AND THE NEW FORMATTED DATASET!

# And make our proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_250.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH propOcc!

# And make and write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_250.csv', row.names = F)

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$year))

length(unique(dataset$species))





