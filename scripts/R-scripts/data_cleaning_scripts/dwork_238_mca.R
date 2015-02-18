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

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*