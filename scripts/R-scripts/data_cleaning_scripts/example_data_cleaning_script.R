################################################################################*
# EXAMPLE DATA CLEANING SCRIPT
################################################################################*

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Get data:

getwd()

list.files('raw_datasets')

d = read.csv('raw_datasets/dataset_223.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset,
# what the fields represent and determining which fields are relavent. Do 
# this carefully, making notes on a piece of scratch paper.

# View field names:

names(d)

# View how many records and fields:

dim(d)

# View the structure of the dataset:

str(d)

# View first 6 rows of the dataset:

head(d)

# View the first 10 rows of the dataset:

head(d, 10)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "d1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(d)

d1 = d[,-c(1,2,8,11,13,14)]

head(d1)

# Because all (and only) the fields we want are present, we can re-assign d1:

d = d1

# View summary of fields in the dataset:

summary(d)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Reminder of the dataset:

head(d)

# We can see that sites are broken up into (potentially) 5 fields. Let's explore
# whether the "site" field itself suffices:

# How many sites are there?

length(unique(d$site))

# How many records are there per site?

ddply(d, .(site), nrow)

# Hmmmm ... it seems the scale of site is off (and a conversation with
# Sevilleta confirmed this). What if we merged all of the site columns?
# Use paste to merge:

site = paste(d$site, d$block, d$treatment, d$plot, d$quad, sep = '')

head(site)

length(unique(site))

# Now we have quite a few sites, how many records are there per site?
# Assign a name, because it's going to be super long:

siteTable = ddply(data.frame(site), .(site), nrow)

head(siteTable)

# Sort the table to see the fewest number of records per site:

head(siteTable[order(siteTable$V1),],10)

# Lot's of sites with few records! Let's explore further:

summary(siteTable)

# Subset to records > 0

d = d[d$cover>0,]

# Explore site data:

site = paste(d$site, d$block, d$treatment, d$plot, d$quad, sep ='')
length(site)
length(unique(site))
table(site)
# Coarser scale sites removing d$quad
site = paste(d$site, d$block, d$treatment, d$plot, sep ='')

# Subsetting data for removing 'NA' and site C3C1 due to lack of sample size

site = subset(site, site!= 'C3C1' & site != 'NANANANA')
length(site)
table(site)

# Add site column to dataset

d$site = paste(d$site, d$block, d$treatment, d$plot, sep ='')
d1 = subset(d, site!= 'C3C1' & site != 'NANANANA')
head(d1)

#Remove unnecessary columns

d = d1[,c(3,9,10,12,15)]
head(d)

# Explore species

length(unique(d$species))
unique(d$species)

# Subset out unwanted species

species = d$species
badspec = c('DEAD', 'seed', '<NA>', 'seeds2', 'seeds1')
species = species[!species%in%badspec]
#instead of long way:
species = subset(species, species!= 'DEAD' & species!= 'seed'& species!= '<NA>' & species!= 'seeds2', species!= 'seeds1')
length(species)
d1 = d[!d$species%in%badspec,]
dim(d1)
head(d1)
unique(d1$species)
d = na.omit(d1)
dim(d)
head(d)

# Time assignment

str(d)
unique(d$season)

#Substringing year

library(stringr)
substr('hello_world',1,5)
str_sub('hello_world',-5)
?str_sub

year = str_sub(d$season, -4)
head(year)
year = as.numeric(year)
head(year)

# For FALL and SPRING vectors

season = str_sub(d$season, end = -5)
head(season)
tail(season)

# Putting year + season together

season1 = ifelse(season == 'SPRING',.25,.75)
str(season1)
summary(season1)
year = year + season1
head(year)

# Extracting date METHOD 2 making date objects

head(d)
date = strptime(d$record_record_date, '%m/%d/%Y')
head(date)
year2 = as.numeric(format(date, '%Y'))
head(year2)
month = as.numeric(format(date, '%m'))
head(month)
unique(month)
quarter = ifelse(month >= 9,.75,.25)
summary(quarter)
head(quarter)

d$year = year
head(d)
d1 = d[,-c(2,5)]
head(d1)

# Count 

library(plyr)
d = ddply(d1,.(site, year, species), summarize, count = max(cover))
head(d)
dim(d)
summary(d)

# Dataset ID assignment
dim(d)
d$datasetID = rep(223,length(d[,1]))
head(d)
d = d[,c(5,1,3,2,4)]
head(d)
names(d)[1] = 'datasetID'

# Writing dataframe to main file

write.csv(d, "formatted_datasets/dataset_223.csv",row.names = F)
