# Cleaning dataset 173 OBIS Marine Inverts

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
setwd("C:/Users/auriemma/core-transient/")
source('scripts/R-scripts/core-transient_functions.R')

# Get data

ds = 173 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Explore

dim(dataset)
names(dataset)
str(dataset)
head(dataset)
summary(dataset)

# Remove unwanted columns
  # List removed fields

unusedFields = c(1,2)
dataset1 = dataset[,-unusedFields]

# Check

dim(dataset1)
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!



#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore
length(unique(d$SampleID))

  # 758 individual sites
class(d$SampleID)

# View unique sites for any unwanted site names (NAs, undefined, etc)
levels(d$SampleID)

  # No unwanted site names to remove

# Change name
names(d)
names(d)[3] = 'site'
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore species
length(unique(d$Species))
  # 678 different species accounted for

# Capitalize all species to eliminate letter case repeats
d$sp = toupper(d$Species)
length(unique(d$sp))
  # Still 678 unique spp names, so no errors
  #Remove sp column
head(d)
d = d[,-6]

# Look for unidentifieds or other unwanted species
levels(d$Species)
  # No questionable species or unidentifieds
  # wide variety in taxonomic resolution
class(d$Species)

# Change name
names(d)[4]= 'species'
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
head(d)
summary(d$Year)
class(d$Year)

# Change to factor 
d1 = d 
d1$Year = factor(as.character(d1$Year))

# Explore for bad values
levels(d1$Year)

# All good, change name and back to d
names(d1)[2]= 'date'
d = d1
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore
summary(d$Abundance)
  # No zeros
class(d$Abundance)

# Change to numeric
d1 = d
d1$Abundance = as.numeric(as.character(d1$Abundance))
class(d1$Abundance)
summary(d1$Abundance)

# Look for bad count values
unique(d1$Abundance)
  # No bad values, none removed

# Change column name
names(d1)[5] = 'count'

head(d1)

# revert back to d
d = d1
head(d, 30)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Change name of datasetID
names(d)[1] = 'datasetID'
head(d)

  #Change to character
d$datasetID = as.character(d$datasetID)

# Check data structure
str(d)

# Make the dataframe
d1 = ddply(d, .(datasetID, site, date, species), summarize, count = max(count))

# Explore new dataframe
head(d1, 40)
summary(d1)
str(d1)

# All looks good, revert back to d
d = d1

head(d)

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# final look at the dataset:

head(d)
summary(d)

# looks okay, write formatted data frame

write.csv(d, "data/formatted_datasets/dataset_173.csv", row.names = F)

################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*
library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

d = read.csv("data/formatted_datasets/dataset_173.csv")

head(d)

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Year is default temporal grain size

# change date column to factor
d$date = factor(as.character(d$date))
class(d$date)

# Temporal grain for this dataset is already year

# Change column name:

names(d)[3] = 'year'
head(d, 30)

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*

# How many sites are there?
length(unique(d$site))
  # 758 unique sites, so likely several with low number of records

# How many species and time samples per site
siteTable = ddply(d, .(site), summarize, nyear= length(unique(year)), 
                  nsp = length(unique(species)))
head(siteTable, 20)
tail(siteTable)
summary(siteTable)

# All sites have only 1 time sample and several have less than 10 spp
# Need to expand the scope of each site 
# Look at all sites to find consistencies across all data

unique(d$site)

# All sites are listed by given name, and most followed by numeric 
# (2-4 digits) site name as well. All words and numeric indicators are
# separated by underscores.

# After looking through all site names, the ones that appear several times
# are the ones that have different numeric indicators at the end of the name.
# So a good way to expand the scope would be to substring the last 4 characters
# of the sites to remove numbers and get just the names.

site1 = str_sub(site, end = -4)
head(site1, 30)
head(site, 30)
tail(site1, 30)
# There were 2 spaces after the numbers in sites, so remove 2 more characters
site1 = str_sub(site, end = -6)
head(site1, 50)

# it worked, now plug new sites into dataset
d1 = d
d1$site = site1
head(d1, 20)

# Now check siteTable for year and species sample sizes per site
siteTable = ddply(d1, .(site), summarize, nyear = length(unique(year)),
                                          nsp = length(unique(species)))
head(siteTable, 20)
head(siteTable[order(siteTable$nyear),], 30)
tail(siteTable[order(siteTable$nyear),], 30)
length(unique(site1))

