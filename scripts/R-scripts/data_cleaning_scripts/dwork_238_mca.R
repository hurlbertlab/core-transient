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

d = read.csv('data/raw_datasets/dataset_238.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Check dimensions, fields, etc
dim(d)
names(d)
summary(d)

# Dataset is in wide format organized by date of record (34 different recorded dates)
# Need to re-format data so is in long form
  # Use 'reshape2' package and melt function
library('reshape2')
d.long = melt(d,id.vars = c('record_id','species','trt','grid'))
head(d.long, 30)

# It worked, but can now remove unwanted columns and call it 'd'
d = d.long[,-c(1,3)]
head(d)

# Explore reshaped data structure
str(d)
# check 'variable' column to see if any dates were lost after reshape
length(unique(d$variable))
  # no, still 34 dates

# Change names to appropriate 
names(d) = c('species','site','date','count')
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore unique sites
class(d$site)

# Change sites to factor
d$site = factor(as.character(d$site))
class(d$site)
head(d)
length(unique(d$site))
unique(d$site)

  # 16 different sites labeled 1 thru 16

# See how many records there are per site
siterecs = table(data.frame(d$site))
siterecs
  # Same amount of records for each, 1496 records per site
# check random spot in data for format
d[300:350,]
# Site data appears all good

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Explore species
length(unique(d$species))
  # 44 Unique coded species
unique(d$species)

# Remove unwanted species
badsp = c('OTHERS')
d1 = d[!d$species %in% badsp,]
unique(d1$species)
length(unique(d1$species))
  # Worked, now only 43 unique species

# How many lost rows
nrow(d)
nrow(d1)

d = d1

# Explored the coding of species in this study and found they
# are lised as just 4 letter abbreviations for speciefic species 
# within the experimental plots.  All species shown here were 
# confirmed and all bad spp were removed.

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore dates and format
class(d$date)
levels(d$date)

# Separate month and year
month = str_sub(d$date, end = 3)
unique(month)
  #change to factor
month = factor(month)
levels(month)

year = str_sub(d$date, start = -2, end = -1)
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
d1$date = paste(month, year, sep = '/')
head(d1,20)
tail(d1, 20)

# change date to date format
  # Make dates as.character first
date = factor(d1$date)
head(date)
class(date)
date2 = strptime(date, format = "%m/%Y")
class(date2)
head(date2)
tail(date2)
# !!!This strptime DID NOT WORK without day.(returned all NAs) 
# so need to add first of month (vector of 1s) to do %d/%m/%Y format
day = rep(1, nrow(d1))
d1$date = paste(day,month,year,sep = "/")
head(d1)

# change to date format
date = as.character(d1$date)
date1 = strptime(date, "%d/%m/%Y")
head(date1)
tail(date1)
class(date1)
# This worked, so made note in data source table

# Replace with this new date column
head(d1)
d1$date = date1
head(d1)
str(d1)
summary(d1)
unique(d1$date)

# Revert back to d
d = d1
head(d, 20)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore data
summary(d$count)
summary(d)
length(unique(d$count))
unique(d$count)

# Change to numeric
d$count = as.numeric(d$count)
length(unique(d$count))
unique(d$count)
  # there are NAs and zeros

# Remove NAs and zeros
d = d[d$count>0,]
length(unique(d$count))
head(d)
dim(d1)
dim(d)

d = na.omit(d)
dim(d)
length(unique(d$count))

# Check 
summary(d)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Add dataset ID column
d$datasetID = rep(238, nrow(d))
head(d,10)

# Make dataframe
str(d)
  # Change date to factor
d$date = factor(as.character(d$date))
head(d)
d1 = ddply(d, .(datasetID, site, date, species), summarize, count = max(count))

# Check the dataframe
summary(d1)
head(d1, 30)

# Convert date back to date object
d1$date = as.Date(d1$date)
head(d1)
summary(d1)

# All looks good
d = d1
#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*
summary(d)
head(d,20)

# Write to data submodule
setwd('C:/Users/auriemma/core-transient/')
write.csv(d, "data/formatted_datasets/dataset_238.csv", row.names = F)

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

length(unique(d$species))

################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

d = read.csv("data/formatted_datasets/dataset_238.csv")

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
head(d, 40)
length(unique(d$date))
unique(d$date)\

# Temporal grain is 3 records per year, so turn into decimal years
# Extract year values:
  # First need to turn d$date into date object
d$date = as.Date(d$date)
year = as.numeric(format(d$date, '%Y'))
unique(year)
summary(year)

# Extract month values (if applicable):

month = as.numeric(format(d$date, '%m'))
head(month)
summary(month)

# Make months decimals
dec_month = (month - 1)/12
unique(dec_month)

# Add dec_month and year to get decimal year
decimalyear = year + dec_month
head(decimalyear, 40)
d$date = decimalyear
head(d, 40)

# Name column 'year'
names(d)[3] = 'year'
head(d)

# Create new dataframe
d1 = ddply(d, .(datasetID, site, year, species), 
                 summarize, count = max(count))
# Explore 
head(d1)
summary(d1)
str(d1)
str(d)

# all looks good
d = d1

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*

# How many sites are there?

length(unique(d$site))

# How many time and species records are there per site?

siteTable = ddply(d, .(site), summarize, nYear = length(unique(date)),
                  nSp = length(unique(species)))
head(siteTable, 30)

# 31 time samples per site, and > 30 species per site

# So no problems with site scale in this dataset, no bad sites to remove

