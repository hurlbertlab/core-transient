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
d = d[,-c(5,6,9)]
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
names(d)[4] = "species"

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
str(d)
summary(d)
unique(d$Adults)

# Change to numeric
d$Adult = as.character(d$Adult)
d$Adult = as.numeric(d$Adult)

# Remove NAs
d = na.omit(d)
unique(d$Adults)

# Change name from adults to count
names(d)[5] = "count"
head(d)
