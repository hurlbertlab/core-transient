# Cleaning dataset 173 OBIS Inverts

# Set Up
  # Load libraries and source functions
setwd("C:/Users/auriemma/core-transient/")

library(plyr)
library(stringr)
source("scripts/R-scripts/core-transient_functions.R")

# Get data
d = read.csv("data/raw_datasets/dataset_173.csv")

# Explore dataset
dim(d)
names(d)
str(d)
head(d)
summary(d)

# Remove unwanted columns
d = d[,-c(1)]
head(d)

# Rename columns to appropriate names
names(d) = c('datasetID','year','site','species','count')
names(d)
head(d, 15)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore
length(unique(d$site))

  # 758 individual sites
class(d$site)

# View unique sites for any unwanted site names (NAs, undefined, etc)
levels(d$site)

  # No unwanted site names

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore species
length(unique(d$species))
  # 678 different species accounted for

# Capitalize all species to eliminate errors
d$sp = toupper(d$species)
length(unique(d$sp))
  # Still 678 unique spp names, so no errors
  #Remove sp column
head(d)
d = d[,-c(6)]

# Look for unidentifieds or other unwanted species
levels(d$species)
  # No questionable species or unidentifieds
  # wide variety in taxonomic resolution

