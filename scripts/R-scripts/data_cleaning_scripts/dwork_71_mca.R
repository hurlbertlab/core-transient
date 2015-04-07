# Formatting Dataset 71: OBIS ArcOD Plankton

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

source('scripts/R-scripts/core-transient_functions.R')

# Get data. First specify the dataset number ('ds') you are working with.

ds = 71

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)
head(dataset)
str(dataset)
summary(dataset)

# Remove column 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Rename SampleID as site, and ID as datasetID

names(dataset1)[1] = 'datasetID'
names(dataset1)[3] = 'site'
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore

length(unique(dataset1$site))
summary(dataset1)
unique(dataset1$site)

# Site names have A LOT of unnecessary information like species names, sampling methods, etc

# No Lat_longs, just region and possibly a region or site number

dataset2 = dataset1

dataset2$site = factor(dataset2$site)

str(dataset2)

# For now, leaving sites as they are until further decision is made. 

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'region_site_sitenumber_') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N') 

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'Sites have a lot of unnecessary information including species names, catch methods, etc. No changes made to site data, need to find way to remove some unwanted info from sites')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

  # change name first

names(dataset2)[4] = 'species'
class(dataset2$species)
levels(dataset2$species) 

# Change to all uppercase

dataset2$species = toupper(dataset2$species)

# Change to factor

dataset2$species = factor(dataset2$species)

# Look for bad species

levels(dataset2$species)

# One species found with a typo
# Table to see how many are messed up

table(dataset2$species)

# Only 1 individual with the typo and 278 with good spelling, so just remove the bad one

bad_sp = c(' TH+M37663ALASSIOSIRA_ANTARCTICA_BOREALIS')

# Remove

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset the factor levels

dataset3$species = factor(dataset3$species)

# Check

levels(dataset3$species)
nrow(dataset2)
nrow(dataset3)

# # !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. 

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'only one removal of the THALASSIOSIRA_ANTARCTICA_BOREALIS species because of a typo in one individual. All other species good.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore

names(dataset3)

# Fill in the original field name here

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Check for NAs or zeros

summary(dataset3)
str(dataset3)

# No zeros, remove NAs if there are any

dataset4 = na.omit(dataset3)

# No NAs or zeros, set to dataset 5

dataset5 = dataset4

head(dataset5)
unique(dataset5$count)

# Data is not whole numbers

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents abundance count. There were no NAs nor 0s that required removal.  Count data is not in whole numbers, must be concentrations or densities per a certain volume of seawater')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

names(dataset5)
summary(dataset5)

# Data only given by year

datefield = 'Year'

# Make year field numeric

dataset6 = dataset5

dataset6$Year = as.numeric(dataset6$Year)

# change field name

names(dataset6)[2] = 'date'

# Check 

head(dataset6)
str(dataset6)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided as years. The only modification to this field involved converting to a numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# DatasetID already in datase

# Make the compiled dataframe

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore the data frame:

dim(dataset7)

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table
  
dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Take a final look at the dataset:

head(dataset7)

summary(dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_71.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!
