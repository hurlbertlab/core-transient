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

dataset = read.csv('data/raw_datasets/dataset_238.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Check dimensions, fields, etc
dim(dataset)
names(dataset)
summary(dataset)

# Dataset is in wide format organized by date of record (34 different recorded dates)
# Need to re-format data so is in long form
  # Use 'reshape2' package and melt function
library('reshape2')
d.long = melt(dataset,id.vars = c('record_id','species','trt','grid'))
head(d.long, 30)

# It worked, but can now remove unwanted columns and call it 'd'
dataset1 = d.long[,-c(1,3)]
head(dataset1)

# Explore reshaped data structure
str(dataset1)

# check 'variable' column to see if any dates were lost after reshape
length(unique(dataset1$variable))
  # no, still 34 dates

# Change names to appropriate 
names(dataset1) = c('species','site','date','count')

# Revert back to dataset
dataset = dataset1
head(dataset)
rm(d.long)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore unique sites
class(dataset$site)

# Change sites to factor
dataset$site = factor(as.character(dataset$site))
class(dataset$site)
head(dataset)
length(unique(dataset$site))
unique(dataset$site)

  # 16 different sites labeled 1 thru 16

# See how many records there are per site
siterecs = table(data.frame(dataset$site))
siterecs
  # Same amount of records for each, 1496 records per site
# check random spot in data for format
dataset[300:350,]
# Site data appears all good

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore species
length(unique(dataset$species))
  # 44 Unique coded species
unique(dataset$species)

# Remove unwanted species
badsp = c('OTHERS')
dataset1 = dataset[!dataset$species %in% badsp,]
unique(dataset1$species)
length(unique(dataset1$species))
  # Worked, now only 43 unique species

# How many lost rows
nrow(dataset)
nrow(dataset1)

dataset = dataset1

# Explored the coding of species in this study and found they
# are lised as just 4 letter abbreviations for specific species 
# within the experimental plots.  All species shown here were 
# confirmed and all bad spp were removed.

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore dates and format
class(dataset$date)
levels(dataset$date)

# Separate month and year
month = str_sub(dataset$date, end = 3)
unique(month)
  #change to factor
month = factor(month)
levels(month)

year = str_sub(dataset$date, start = -2, end = -1)
class(year)
unique(year)
  # change to factor
year = factor(year)

# Change months to numbers
levels(month)
levels(month)= c('4', '8', '12')
head(month)
month

# change years to full 4 digit year
levels(year)
levels(year) = c("2002", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999")
head(year)
tail(year)

# Add date to dataset
dataset1$date = paste(month, year, sep = '/')
head(dataset1,20)
tail(dataset1, 20)

# change date to date format
  # Make dates as.character first
date = factor(dataset1$date)
head(date)
class(date)
date2 = strptime(date, format = "%m/%Y")
class(date2)
head(date2)
tail(date2)
# !!!This strptime DID NOT WORK without day.(returned all NAs) 
# so need to add first of month (vector of 1s) to do %d/%m/%Y format
day = rep(1, nrow(dataset1))
dataset1$date = paste(day,month,year,sep = "/")
head(dataset1)

# change to date format
date = as.character(dataset1$date)
date1 = strptime(date, "%d/%m/%Y")
head(date1)
tail(date1)
class(date1)
# This worked, so made note in data source table

# Replace with this new date column
head(dataset1)
dataset1$date = date1
head(dataset1)
str(dataset1)
summary(dataset1)
unique(dataset1$date)

# Revert back to d
dataset = dataset1
head(dataset, 20)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore data
summary(dataset$count)
summary(dataset)
length(unique(dataset$count))
unique(dataset$count)

# Change to numeric
dataset$count = as.numeric(dataset$count)
length(unique(dataset$count))
unique(dataset$count)
  # there are NAs and zeros

# Remove NAs and zeros
dataset = dataset[dataset$count>0,]
length(unique(dataset$count))
head(dataset)
dim(dataset1)
dim(dataset)

dataset = na.omit(dataset)
dim(dataset)
length(unique(dataset$count))

# Check 
summary(dataset)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Add dataset ID column
dataset$datasetID = rep(238, nrow(dataset))
head(dataset,10)

# Make dataframe
str(dataset)
  # Change date to factor
dataset$date = factor(as.character(dataset$date))
head(dataset)
dataset1 = ddply(dataset, .(datasetID, site, date, species), summarize, count = max(count))

# Check the dataframe
summary(dataset1)
head(dataset1, 30)

# Convert date back to date object
dataset1$date = as.Date(dataset1$date)
head(dataset1)
summary(dataset1)

# All looks good
dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*
summary(dataset)
head(dataset,20)

# Write to data submodule
setwd('C:/Users/auriemma/core-transient/')
write.csv(dataset, "data/formatted_datasets/dataset_238.csv", row.names = F)


################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*

library(stringr)
library(plyr)
setwd('C:/Users/auriemma/core-transient/')
install.packages('gridExtra')
source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_238.csv")

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# change to Year temporal grain
class(dataset$date)
dataset$date = getYear(dataset$date)

# Change column name to year
names(dataset)[3] = 'year'
head(dataset)

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*

# How many sites are there?

length(unique(dataset$site))

# How many time and species records are there per site?

siteTable = ddply(dataset, .(site), summarize, nYear = length(unique(year)),
                  nSp = length(unique(species)))

# view table
siteTable

# 31 time samples per site, and > 30 species per site

# So no problems with site scale in this dataset, no bad sites to remove

# Re-write the dataset summary with new temporal grain (but no new spacial)
dataset1 = ddply(dataset, .(datasetID, site, year, species), 
                 summarize, count = max(count))
head(dataset1)
summary(dataset1)
dim(dataset)
dim(dataset1)

# All looks good after changes

dataset = dataset1


#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Make proportional occurence data frame:

write.csv(propOccFun(d), "data/propOcc_datasets/propOcc_238.csv", row.names = F)

# Make site summary
write.csv(siteSummaryFun(d), "data/siteSummaries/siteSummary_238.csv", row.names = F)

# Committed and pushed to submodule and core-transient git folder

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

length(unique(d$year))

length(unique(d$species))
