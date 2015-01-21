# Formatting dataset 208: Landis long-term insect database

# Get data and open packages

getwd()
setwd('c:/Users/auriemma/core-transient/raw_datasets')
d = read.csv('dataset_208.csv')
library(plyr)
library(stringr)

# Exploring dataset

head(d)
names(d)
dim(d)
str(d)

# Remove columns not needed

d1 = d[,-c(1,7,8,10,11)]
head(d1)
head(d1, 50)
head(d1, 100)
head(d1,10)
d = d1
summary(d)

#================================================================================
# SITE Column

length(unique(d$treatment))
unique(d$treatment)
length(unique(d$replicate))
unique(d$replicate)
length(unique(d$station))
unique(d$station)

# Remove 'treatment' column
d = d[,-c(2)]
head(d)

# How many rows in each classification of site
ddply(d, .(replicate), nrow)
ddply(d, .(station), nrow)

# Combine replicate and station columns to be specific sites
site = paste(d$replicate, d$station, sep = '')
head(site, 50)

# How many records per site now?
site_table = ddply(data.frame(site), .(site), nrow)
head(site_table)

# Add site column to dataset and remove old site columns
d1$site = site
head(d1)
d = d1[,-c(2,3,4)]
head(d)

#============================================================================
# SPECIES column

  #Uppercase all species to remove any capitalization errors
d$species = toupper(d$species)
sp = d$species
unique(sp)
length(sp)
  
  # Removing unwanted species
remove_sp = c('SOMETHING ELSE',"")
d1 = d[!d$species %in% remove_sp,]
unique(d1$species)
head(d1)
d = d1

#=============================================================================
# COUNT column

head(d)
summary(d)
str(d$adults)
  
  # Create separate count vector
count = d$adults
head(count)
str(count)
  
  # Make all values of 0 and blanks 'NA'
count[count == 0] = NA
head(count)
summary(count)
count[count == ""] = NA
summary(count)

  # Add column to dataset and check if all data lines up correctly
d1$count = count
tail(d1,100)
d1[500:700,]
d1[1300:1600,]
d1[12334:12563,]

  # Remove NA's and 'adults' column
str(d1)
d1 = na.omit(d1)
head(d1)
d = d1[,-c(6)]
head(d,100)

  # Make count column numeric values
d$count1 = as.character(d$count)
head(d)
str(d)
d$count = as.numeric(d$count1)
str(d)
d = d[,-c(6)]
head(d)

#==============================================================================
# YEAR column

  # Make date vector into date object using date column from dataset
date = strptime(d$sample_record_date, '%Y-%m-%d')
class(date)
head(date)

  # Put formatted date column back into dataset
d$date = date
head(d)
str(d)

  #Make date as character for ddply ability
d$date = as.character(d$date)
head(d, 50)
str(d)

  #Remove old date column
d = d[,-c(1)]
head(d)

  # Explore number of records taken per day in dataset
recsperday = ddply(d,.(date), nrow)
head(recsperday[order(recsperday$V1),], 40)

  # Several sites have small sample of records per day, so look at per month
  # Remove day from date vector using substring function
d1 = d
d1$date = str_sub(d1$date, end = -4)
head(d1)

  #Check number of records per month

