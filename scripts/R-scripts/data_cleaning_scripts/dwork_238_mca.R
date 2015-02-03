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
head(site.df[order(site.df$V1),], 16)
    # All sites have same number of records: 1496 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Check number of uniques
length(unique(d$species))

# Look for unwanted species
unique(d$species)
bad_spp = c('OTHERS')
    # Remove species
d1 = d[!d$species%in%bad_spp,]

    # Did it work?
length(unique(d1$species))
unique(d1$species)

    # Yes 43 uniques instead of 44, change d1 to d
d = d1
head(d)
length(unique(d$species))

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore
length(unique(d$value))
class(d$value)
 
# Change to numeric
d$count = as.numeric(d$value)
class(d$count)
  # Warning message: 'NAs introduced by coercion'
unique(d$value)
unique(d$count)

  # Automatically removed character "ND" in value column and kept NAs
  # Confirmed by checking old vs new column 
length(unique(d$value))
length(unique(d$count))

# Remove old column
d = d[,-c(3)]
head(d)

# Remove zeros and NAs
d1 = d[d$count>0,]
length(unique(d1$count))
unique(d1$count)

    # Change back to d and omit NAs
d = na.omit(d1)
length(unique(d$count))
summary(d$count)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
length(unique(d$date))
unique(d$date)
class(d$date)

# Separate month from year
?str_sub
d$month = str_sub(d$date, end = 3)
unique(d$month)
d$year = str_sub(d$date, start = -2, end = -1)
head(d)
class(d$year)
class(d$month)

# Change month to factor
d$month = factor(d$month)
head(d)
str(d)

# Turning into decimal years
d1 = d
head(d1)
?ifelse
d1$month = ifelse(d1$month == "apr",4,ifelse(d1$month == "aug",8,12))
unique(d1$month)

# Other much more efficient and better way of converting to decimal years
d1 = d
levels(d1$month)
levels(d1$month) = c(4,8,12)
head(d1)

  #Need to change to character from factor before converting to numeric
d1$month = as.character(d1$month)
head(d1)
  # Change to numeric
d1$month = as.numeric(d1$month)
str(d1)
d1$month = d1$month/12
head(d1)
unique(d1$month)

# Change year to full year number
  #Change levels
d2 = d1
head(d2)
levels(factor(d2$year))
d2$year = factor(d2$year)
head(d2$year)
levels(d2$year)
levels(d2$year) = c(2002,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999)
unique(d2$year)
head(d2, 100)
tail(d2, 100)

# Add decimal month to year (But dec = 1 so it's going to add 1 to the year??)
  # Do I set it to .99??

