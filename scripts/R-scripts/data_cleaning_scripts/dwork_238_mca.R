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




