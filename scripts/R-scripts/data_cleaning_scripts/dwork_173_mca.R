# Cleaning dataset 173 OBIS Marine Inverts

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
  # Load libraries 
setwd("C:/Users/auriemma/core-transient/")

library(plyr)
library(stringr)

# Source functions
source("scripts/R-scripts/core-transient_functions.R")

# Get data
d = read.csv("data/raw_datasets/dataset_173.csv")

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Explore
dim(d)
names(d)
str(d)
head(d)
summary(d)

# Remove unwanted columns
d = d[,-1]
head(d)

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

summary(d$date)
length(unique(d$date))

length(unique(d$species))
