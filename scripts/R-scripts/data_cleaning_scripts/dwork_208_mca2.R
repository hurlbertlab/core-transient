# Formatting dataset 208: Landis insects

# set correct directory
getwd()
setwd('C:/Users/auriemma/core-transient')

# Library packages and source functions
library(plyr)
library(stringr)
source('scripts/R-scripts/core-transient_functions.R')

# Get data
d = read.csv('data/raw_datasets/dataset_208.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(d)
str(d)
head(d)

# Remove unwanted columns
d = d[,-c(2,5,6,8,9)]
head(d, 10)
summary(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Find number unique species
length(unique(d$Species))
unique(d$Species)

# Remove unwanted species
d1 = d
badspp = c('something else')
d1 = d1[!d1$Species %in% badspp,]
dim(d1)
dim(d)
unique(d1$Species)

d = d1

# Change name
names(d)[3] = "species"
names(d)
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

# Change name from adults to count
names(d)[4] = "count"
head(d)
unique(d$count)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
length(unique(d$Sample_Date))
tail(d$Sample_Date)
class(d$Sample_Date)

# Dates are listed WEEKLY in summer months in format YYYY-MM-DD
levels(d$Sample_Date)

# Separate the year, month, day
d$year= str_sub(d$Sample_Date, end = 4)
d$month = str_sub(d$Sample_Date, start = 6, end = 7)
d$day= str_sub(d$Sample_Date, start = -2, end =-1)
head(d)
str(d)

# Change name
names(d)[1] = "date"

# Convert all to numeric
d$year = as.numeric(d$year)
d$month = as.numeric(d$month)
d$day = as.numeric(d$day)

# Turn months into day of year by month
d$dayofyear = ((d$month-1)*30)+d$day
head(d, 20)
tail(d, 20)

# Convert day of year to decimal
d$dec_day = d$dayofyear/365
head(d, 20)
tail(d, 20)

# Get week of the year
d$weekofyear = d$dayofyear/7
head(d)

# Convert to decimal week
d$dec_week = d$weekofyear/52
head(d)

# Convert to decimal-year by adding decimal week to year
d1 = d
head(d1)
d1$year = d1$year+d1$dec_week
head(d1 ,20)
tail(d1, 20)

d = d1

# Remove other time columns (d1 will still have them)
d = d[,c(2,3,4,5)]
head(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore
length(unique(d$Replicate_Station))
# 30 unique sites
  # Change name to site
names(d)[2] = "site"

unique(d$site)

# No bad sites

# Check number of records for each site
siteTable = ddply(d, .(site), nrow)
siteTable
head(siteTable[order(siteTable$V1),],10)
  # Sufficient sample size for each site (>1300 for each)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# Add datasetID number
d$datasetID = rep(208, nrow(d))
head(d)

# Make the dataframe
d.df = ddply(d,.(datasetID, site, year, species), summarize, count = max(count))
head(d.df,20)
summary(d.df)

# Check sites for enough time samples and richness
siteTable = siteSummaryFun(d.df)
head(siteTable)
dim(siteTable)
siteTable

badSites = badSiteFun(d.df)
dim(badSites)
  # No sites to remove
head(d.df, 50)
tail(d.df, 50)

