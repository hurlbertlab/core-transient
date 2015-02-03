# Formatting dataset 238 Terrestrial plants metadata

# Open libraries
library(plyr)
library(reshape2)
library(stringr)

# Source data cleaning functions
getwd()
setwd("C:/Users/auriemma/core-transient")
source('scripts/R-scripts/core-transient_functions.R')

# Get data
d = read.csv('raw_datasets/dataset_238.csv')

# Explore
dim(d)
names(d)
str(d)

# Already can tell it is in wide format with dates as indiv columns
# Change to long format
d.long = melt(d,id.vars = c('record_id','species','trt','grid'))
head(d.long, 50)
# Check class of newly structured data
class(d.long$value)
class(d.long$variable)
  
# Change date variable to character
d.long$date = as.character(d.long$variable)
head(d.long)
summary(d.long)

# See if any dates were removed after melt
length(unique(d.long$variable))
  # same number of unique dates so none were removed, revert 'd.long' to 'd'
d = d.long

#Remove unwanted columns
d = d[,-c(1,3,5)]
head(d)

# give appropriate name to site column
names(d)[2] = 'site'
head(d)

summary(d)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Check for number of sites
length(unique(d$site))
unique(d$site)

# Create site vector
site = d$site

# Sites seem to be broken down as grid numbers 1 thru 16, with no other specification
# How many records are there per site
site.df = ddply(data.frame(site), .(site), nrow)
head(site.df[order(site.df$V1),], 10)
    # All sites have same number of records: 1496 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Check number of uniques
length(unique(d$species))

# Look for unwanted species
bad_spp = c('OTHERS')
    # Remove species
d1 = d[!d$species%in%bad_spp,]

    # Did it work?
length(unique(d1$species))
unique(d1$species)

    # Yes 43 uniques instead of 44
d = d1
head(d)


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore
length(unique(d$value))
class(d$value)
 
# Change to numeric
d$count = as.numeric(d$value)
  # Warning message: 'NAs introduced by coercion'??
