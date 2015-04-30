## Formatting Dataset 208: Landis Long Term Insect Dataset

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

setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

# Get data

ds = 208 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
str(dataset)
names(dataset)

# Remove unused columns

unusedFields = c(2,5,6,8,9)

dataset1 = dataset[,-unusedFields]
names(dataset1)
head(dataset1)

# Change field names

names(dataset1)[c(1,2,3)] = c('date','site','species')
names(dataset1)
head(dataset1)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore

length(unique(dataset1$site))
unique(dataset1$site)

# Sites are concatenated (in data before reading into R) as Replicate_Station.

# No changes to be made to sites

dataset2 = dataset1

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'Replicate_Station') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','Y') 

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  'site fields concatenated in data before reading into R. .')

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

################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*

library(stringr)
library(plyr)

getwd()
setwd('C:/Users/auriemma/core-transient/')
source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_208.csv")

head(dataset)

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Change to temporal grain default of year

# Change date column to year:
dataset$date = getYear(dataset$date)
summary(dataset)
head(dataset)

# Change column name:

names(dataset)[3] = 'year'

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# How many sites are there?
length(unique(d$site))
# only 30 different site

# Find time and species sample sizes
siteTable = ddply(dataset, .(site), summarize,
                  nyear = length(unique(year)),
                  nsp = length(unique(species)))
# View table
siteTable

# All sites have adequate sample sizes (>5 Years, >10 species)

# Double check for bad sites
badSites = subset(siteSummaryFun(d), spRich < 10 | nTime < 5)$site
length(badSites)

# Length = 0, so no bad sites

# Re-write the dataset summary with new temporal grain (no spacial grain change)
dataset1 = ddply(dataset, .(datasetID, site, year, species), summarize, count = max(count))

# Explore new data summary

dim(dataset1)
summary(dataset1)
head(dataset1, 20)

# All good, revert back to dataset
dataset = dataset1

# Explore more

head(propOccFun(dataset), 20)
head(siteSummaryFun(dataset), 20)
summary(siteSummaryFun(dataset))

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Make proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_208.csv", 
          row.names = F)

# site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_208.csv', 
          row.names = F)

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
