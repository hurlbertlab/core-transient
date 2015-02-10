# Formatting Dataset 97 ArcOD Plankton

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
# Load libraries:

install.packages('stringr')
library('stringr')
install.packages('plyr')
library(plyr)

# Source the functions file:
getwd()
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

d = read.csv('data/raw_datasets/dataset_97.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(d)
names(d)
str(d)

# Remove unwanted columns
d1 = d[,-c(1,2)]
head(d1)

  # Change d1 back to d
d = d1
summary(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Sites are recorded as collection method with lat_longs
  # Explore
length(unique(d$SampleID))
class(d$SampleID)
  # Total of 835 unique sites

# Find number of records per site
siteTable = ddply(data.frame(d$SampleID),.(d$SampleID), nrow)
head(siteTable[order(siteTable$V1),],150)
tail(siteTable[order(siteTable$V1),],150)
summary(siteTable)

# Hundreds of sites have very low sample sizes
  ## Need to find a way to broaden site data
  # Use substring to get just lat_lons for each site sample???
class(d$SampleID)
site = d$SampleID
head(site)
site = str_sub(site, start = -20, end = -1)
head(site)
tail(site)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore
length(unique(d$Species))
  # 419 unique species
class(d$Species)

# Capitalize all to remove capitalization error
d$species = factor(toupper(d$Species))
length(unique(d$species))
    # No capitalization errors, none to remove

# Explore unique species
levels(d$species)
  # Wide variation in taxonomic resolution
  # No questionable species recorded, so none removed
names(d)[3] = 'species'

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore
head(d)
length(unique(d$Abundance))
class(d$Abundance)
summary(d$Abundance)

# Already numeric, so remove zeros
d1 = d
d1 = d[d$Abundance>0,]
head(d1)
unique(d1$Abundance)

# No numbers to remove, name Abundance column 'count'
d = d1
names(d)[4] = 'count'
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
length(unique(d$Year))
class(d$Year)
unique(d$Year)

# Change to numeric
d$Year = as.character(d$Year)
d$Year = as.numeric(d$Year)
head(d)
names(d)[1] = 'year'

unique(d$year)
