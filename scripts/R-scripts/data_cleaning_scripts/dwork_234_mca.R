# Cleaning dataset 234: Powdermill Mammals

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*
# # Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

# Source the functions file:

getwd()

source('scripts/R-scripts/core-transient_functions.R')

# Get data

ds = 234

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
names(dataset)
head(dataset)
tail(dataset)
str(dataset)

# Remove unwanted columns

# Lots of them, so list ones that will be used instead of unused
  # List used fields
usedFields = c(3,6,7)

  # Remove
dataset1 = dataset[,usedFields]

# Check
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# View summary of fields in the dataset:

summary(dataset1)
head(dataset1)

# Make 'site' object

site = dataset1$quadr
head(site)

# Check uniques 

levels(site)

# There is a site that is listed as just a blank, check to see if it is a typo

siteTable = data.frame(table(site))
siteTable

# Plenty of records for that site, so give this site the name 'blank'

site1 = as.character(site)
site1[site1 == ""] = 'blank'

unique(site1)

# Add new column to dataset

dataset2 = dataset1

dataset2$site = factor(site1)
head(dataset2)
tail(dataset2)

# Remove old one

dataset2 = dataset2[,-c(3)]

# Check

head(dataset2)

# Check for number of sites sampled per year

uniqQuadPerYear = data.frame(table(unique(dataset[, c('year', 'quadr')])$year))

uniqQuadPerYear

# Note the minor difference in number of sites sampled each year

# Sites are different quadrats within a 1 ha grid, so can be reduced to the whole grid

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'quadr')


dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N')

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites are quadrats within a 1 ha plot. One site was listed as a blank, so gave it the name blank. Site data was checked for variation in number of sites sampled per year.  Varied from 99 sites to 105 sites, a negligible differece, so not important.  Because sites are within a 1 ha plot, this whole dataset can be looked at as only one site.')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Look at the individual species present:

head(dataset2)
levels(dataset2$species)

# Species are coded but metadata found does not discuss species coding.  There are some obvious items to remove from the dataset, though:

bad_sp = c("","?")

# Remove the bad species from dataset

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset factor levels

dataset3$species = factor(dataset3$species)

# Check

levels(dataset3$species)
head(dataset3)

dim(dataset2)
dim(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat.
dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no info in metadata about species coding but not many different species so little chance for error. Only items removed were blanks and items labeled with a question mark')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

names(dataset3)
head(dataset3, 30)

# Leaves just species, date, and site columns

# Species are accounted for several times at one time sample, so extract count data using table function

dataset_count = data.frame(table(dataset3[,c('species','date','site')]))
dataset_count = dataset_count[dataset_count$Freq!=0, ]
head(dataset_count,30)
tail(dataset_count,30)

# Freq value is the number of each species at each site and each time sample

# Change dataset to new dataframe

dataset4 = dataset_count

# Change count column name

names(dataset4)[4] = 'count'

# check all

head(dataset4)
summary(dataset4)

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'data was extracted by creating table and getting frequency of each species at each time sample per each site.')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# This dataset gives date without any separation betw values
# So, need to separate values using substring, then put back together to make date object

dataset5 = dataset4

# Substring year, month, and day

year = str_sub(dataset5$date, end = 4)

month = str_sub(dataset5$date, start = 5, end = 6)

day = str_sub(dataset5$date, start = 7, end = 8)

# Paste values back together

date = paste(month, day, year, sep = '/')

# Change to date format

date1 = strptime(date, '%m/%d/%Y')

class(date1)
summary(date1)

# Add new date field to dataset

dataset6 = dataset5

dataset6$date = date1

# Check

head(dataset6, 20)
tail(dataset6)
str(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  'temporal data provided as dates, but with no separation betw year, month, day values. Separated the values, pasted back together, then converted to date object.  Then added the date object to the dataset.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'Y')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# add the datasetID:

dataset6$datasetID = ds
head(dataset6)
dim(dataset6)
summary(dataset6)

# Now make the compiled dataframe:

class(dataset6$date)

# Change from POSIXlt to POSIXct format

dataset6$date = as.POSIXct(strptime(dataset6$date, '%Y-%m-%d'))

# Make new summary dataset

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = sum(count))

# Check and explore

head(dataset7)
summary(dataset7)
str(dataset7)

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table

dataFormattingTable = dataFormattingTableUpdate(ds)

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_234.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# As we've now successfully created the formatted dataset, we will now update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority', 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag',   1)

# And update the data formatting table:

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))

dataset7 = read.csv("data/formatted_datasets/dataset_234.csv")

dim(dataset7)
length(unique(dataset7$species))

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

datasetID = ds

# Get formatted dataset:

dataset = read.csv(paste("data/formatted_datasets/dataset_",
                         datasetID, ".csv", sep =''))

# Have a look at the dimensions of the dataset and number of sites:

dim(dataset)
length(unique(dataset$site))
length(unique(dataset$date))
head(dataset)

# I can see the some sites are listed as "?" ... removing those:

dataset = subset(dataset, site!= "?")

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check relevant table values:

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'quad', 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 'quad', temporalGrain = 'year',
                              minNTime = 10, minSpRich = 10,
                              proportionalThreshold = .5)

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))
