## Formatting Dataset 238: Plants in Chile

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:

setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

d = read.csv('data/raw_datasets/dataset_208.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
head(d)
str(d)
names(d)

# Remove unwanted columns
d1 = d[,-c(2,5,6,8,9)]
names(d1)
d = d1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore
length(unique(d$Replicate_Station))
unique(d$Replicate_Station)
class(d$Replicate_Station)

# Change name
names(d)[2] = "site"
head (d)
unique(d$site)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Find number unique species
length(unique(d$Species))
unique(d$Species)

# Remove unwanted species
badspp = c('something else')
d1 = d[!d$Species %in% badspp,]
dim(d1)
dim(d)
unique(d1$Species)

d = d1

# Change name
names(d)[3] = "species"
names(d)
unique(d$species)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
str(d)
summary(d)
unique(d$Adults)

# Change to numeric
d$Adults = as.character(d$Adults)
d$Adults = as.numeric(d$Adults)
str(d)

# Remove NAs
d = na.omit(d)
unique(d$Adults)
length(unique(d$Adults))

# Change name from adults to count
names(d)[4] = "count"
head(d)
unique(d$count)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
length(unique(d$Sample_Date))
head(d$Sample_Date)
tail(d$Sample_Date)
class(d$Sample_Date)

# Change date column to date format
d$date = strptime(d$Sample_Date, "%Y-%m-%d")
head(d)
class(d$date)

# Worked so remove old column
d = d[,-1]
head(d)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
head(d)
# Add datasetID column
d$datasetID = rep(208, nrow(d))
head(d)

# check over dataset
summary(d)

# Change date back to factor
d$date = factor(as.character(d$date))
str(d)

# Make dataframe
d2 = ddply(d,.(datasetID, site, date, species), summarize, count = max(count))

# Explore dataframe
head(d2, 30)
summary(d2)

# Change date back to date object
d2$date = as.Date(d2$date)
head(d2, 40)
summary(d2)

# All good, revert back to d
d = d2

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*
head(d)
class(d$date)

# Write it
write.csv(d, "data/formatted_datasets/dataset_208.csv", row.names = F)

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
