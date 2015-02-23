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

