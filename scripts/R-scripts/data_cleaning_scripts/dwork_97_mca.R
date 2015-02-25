Formatting Dataset 97: ArcOD Plankton

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:

setwd("C:/Users/auriemma/core-transient/")
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

d = read.csv('data/raw_datasets/dataset_97.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(d)
head(d)
dim(d)
str(d)

# Remove unwanted column
d = d[,-1]
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore 
length(unique(d$SampleID))
  # 835 Unique sites, listed by station, catch method, and lat_longs in each record

# Search for possible NAs or the like
levels(d$SampleID)

# No bad sites removed, but probably very low sample size for many

# Change name
names(d)[3] = "site"
head (d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore
head(d)

# how many different species?
length(unique(d$Species))
  # 419 unique spp names

# Capitalize to check for error in letter case
d$species = toupper(d$Species)

# Check for difference
length(unique(d$species))
  # No difference, so remove new column
head(d)
d = d[,-6]
head(d)

# Look for bad species
levels(d$Species)

# No bad species found, none removed from data

# change name
head(d)
names(d)[4] = 'species'

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore
head(d)
summary(d$Abundance)
class(d$Abundance)
length(unique(d$Abundance))

# Remove zeros
d1 = subset(d, Abundance > 0)

# Check uniques
d1$count = factor(d$Abundance)
levels(d1$count)

# No values of zero or NAs found

# Remove new column
head(d1)
d1 = d1[,-6]
head(d1)

# Change name
names(d1)[5] = 'count'
names(d1)
head(d1)

d = d1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
head(d)
summary(d$Year)
class(d$Year)

# Change to numeric
d1$Year = as.character(d$Year)
head(d1)
d1$Year = as.numeric(d$Year)
head(d1)
summary(d1)

# Change name and revert back to d
names(d1)[2] = 'date'
head(d1)

d = d1

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Change name of datasetID
names(d)[1]= 'datasetID'
summary(d$datasetID)
class(d$datasetID)

  # Change to character
d$datasetID = as.character(d$datasetID)
head(d)

# Make dateframe
  # First change date to factor
d$date = factor(d$date)
# ddply dateframe
d1 = ddply(d, .(datasetID, site, date, species), summarize, count = max(count))

# Explore new d1
head(d1, 40)
dim(d1)
summary(d1)

# All looks good, set back to d
d = d1

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*
# final look at the dataset:
head(dataset)
summary (dataset)

# Write to data submodule
write.csv(d, "data/formatted_datasets/dataset_97.csv", row.names = F)

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

dim(d)

length(unique(d$site))

length(unique(d$date))
summary(d$date)

length(unique(d$species))

################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*

library(stringr)
library(plyr)
setwd("C:/Users/auriemma/core-transient/")
source('scripts/R-scripts/core-transient_functions.R')

d = read.csv("data/formatted_datasets/dataset_97.csv")

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
head(d)

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Temporal grain for this dataset is yearly, so no changes need to be made for time data
head(d)

# Change to 'year' column
names(d)[3] = 'year'

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# Explore
length(unique(d$site))
  # 835 unique sites, so most likely going to be bad sites

# Check records for each site
siteTable = ddply(d, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable[order(siteTable$nSp),], 30)
tail(siteTable[order(siteTable$nSp),], 30)

# Lot of 1's for nYear and 1-4's for nSp
# How many have less than 10 species?
nrow(subset(siteTable, nSp < 10))
  # There are 180 sites that have less than 10 spp

# How many have less than 5 time samples?
nrow(subset(siteTable, nYear < 5))

#!! No sites have more than 1 time sample!
# Since site data records are catch method, net mesh size, and lat_longs,
# need to extract the lat_long data from it then expand the grain size 

# First thing is to get rid of some unneccesary data using substring
d1 = d
sitedata = data.frame(d1$site)
head(sitedata)
names(sitedata) = 'site'
head(sitedata, 50)
tail(sitedata, 50)
sitedata1 = str_sub(sitedata$site, start = -16)
head(sitedata1, 20)
tail(sitedata1, 20)
summary(sitedata1)

# Parse the values that are separated by underscores to extract latlongs
sitesep = read.table(text = sitedata1, sep = '_')
# No good, need to use stringsplit function

# Strsplit the data in sitedata
head(sitedata1)
sitedata.df = data.frame(sitedata1)
?str_split_fixed
siteSplit = str_split_fixed(sitedata1, '_', 6)
head(siteSplit, 40)
tail(siteSplit, 40)

# Worked, separating the individual strings, but lat longs are in different
# columns for different data groupings. Need to get all in same columns

siteSplit.df = data.frame(siteSplit)
head(siteSplit.df, 30)
names(siteSplit.df) = c('1','2','3','4','5','6')
siteSplit.df

# Write the table to excel to explore better
write.csv(siteSplit.df, 'C:/Users/auriemma/mca-core-trans-work/siteSplit.csv', row.names = F)

# Need to find a way to group lat longs in all same columns