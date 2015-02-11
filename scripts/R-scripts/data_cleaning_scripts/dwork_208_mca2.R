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

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
length(unique(d$Sample_Date))
tail(d$Sample_Date)
class(d$Sample_Date)

# Dates are listed by year-month-day
# Substring to remove day
d$year_mon = str_sub(d$Sample_Date, end = 7)
  # Check to see if it worked for all records
head(d, 40)
d[300:400,]

# Better way of separating month and year
d$month = str_sub(d$Sample_Date, start = 6, end = 7)
head(d,30)
d$year = str_sub(d$Sample_Date, end = 4)
head(d)

# delete year_mon column and old sample_date column
d1 = d[,-c(1,8)]
head(d1)
str(d1)

