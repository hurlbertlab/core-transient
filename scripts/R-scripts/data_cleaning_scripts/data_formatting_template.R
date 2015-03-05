################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Source the functions file:

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_223.csv')

#===============================================================================*
# MAKE FORMATTED DATASET
#===============================================================================*
# The goal is: 
# 1) To create a dataset of the columns of interest at the smallest spatial
#  and temporal sampling grain available.
# 2) To eliminate "bad" species data.

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset,
# what the fields represent and determining which fields are relavent. Do 
# this carefully, making notes on a piece of scratch paper.

# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:

str(dataset)

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "d1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[,-c(5,6,8,9)]

head(dataset1)

# Because all (and only) the fields we want are present, we can re-assign d1:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# View summary of fields in the dataset:

summary(dataset)

# Reminder of the dataset:

head(dataset)

# We can see that sites are broken up into (potentially) 5 fields. Find the 
# metadata link in the data source table use that link to determine how
# sites are characterized.
#  -- If sampling is nested (e.g., site, block, treatment, plot, quad as in 
# this study), use each of the identifying fields and separate each field with
# an underscore.
# -- If sites are listed as lats and longs, use the finest available grain 
# and separate lat and long fields with an underscore.
# -- If the site definition is clear, make a new site column as necessary.

# Here, we will concatenate all of the potential fields that describe the 
# site:

head(dataset)

site = paste(dataset$site, dataset$block, dataset$treatment, 
             dataset$plot, dataset$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset in case 
# you make a mistake:

dataset1 = dataset

dataset1$site = factor(site)

dataset1 = dataset1[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset1)

# All looks good, so overwrite the dataset file:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do
# so, you need to look at the list of unique species very carefully. Avoid being
# too liberal in interpretation, if you notice an entry that MIGHT be a problem, 
# but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

sp = dataset$species

levels(sp) # Note: You can also use unique(sp) here.

# The first thing that I notice is that there are lower and upper case
# entries. Because R is case-sensitive, this will be coded as separate species.
# Modify this prior to continuing:

dataset$species = toupper(dataset$species)

# Let's explore whether there was a difference:

length(unique(dataset$species))

length(unique(sp))

# We see that almost 70 species were the result of upper and lower case!
# Make a new species vector (factor ensures that it is coded as a factor
# rather than character and removes any unused levels) 
# and continue exploring:

sp = factor(dataset$species)

levels(sp)

# Now explore the listed species themselves. To do so, you should go back to study's 
# metadata. A quick look at the metadata is not informative, unfortunately. Because of
# this, you should really stop here and post an issue on GitHub. With some more thorough
# digging, however, I've found the names represent "Kartez codes". Several species can
# be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta
# study (dataset 254) that provides species names for some codes). Some codes were identified
# with this pdf from White Sands: 
# https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('', 'NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE',
  'UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED',
  'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

dataset1 = dataset[!dataset$species %in% bad_sp,]

dataset1$species = factor(dataset1$species)

# Let's look at how the removal of bad species altered the length of the dataset:

nrow(dataset)

nrow(dataset1)

# Look at the head of the dataset to ensure everything is correct:

head(dataset1)

# Having checked through the results, we can now reassign the dataset:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# For starters, let's change the date column to a true date (and give the darned
# column a better name:

head(dataset)

date = strptime(dataset$record_record_date, '%m/%d/%Y')

# A check on the structure lets you know that date field is now a date object:

class(dataset$record_record_date)

class(date)

# Give a double-check, if everything looks okay, then replace the column:

head(dataset$record_record_date)

head(date)

dataset1 = dataset

dataset1$record_record_date = date

names(dataset1)[5] = 'date'

# Let's remove the season field (for now):

dataset1 = dataset1[,-2]
  
# After a check of dataset1, you can rename it dataset:
  
head(dataset)

head(dataset1)

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(dataset)

# Subset to records > 0 (if applicable):

dataset1 = subset(dataset, cover > 0) 

summary(dataset1)

# Remove NA's:

dataset1 = na.omit(dataset1)

# Make sure to write in the data summary table the type of observed count (here,
# it represents % cover)

# How does it look? If you approve,  assign changes to dataset:

summary(dataset)
summary(dataset1)

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, lets add the datasetID:

dataset1 = dataset

dataset1$datasetID = rep(223,nrow(dataset1))

# Change date to a factor:

dataset1$date = factor(as.character(dataset1$date))

# Now make the compiled dataframe:

dataset2 = ddply(dataset1,.(datasetID, site, date, species),
                 summarize, count = max(cover))


# Explore the data frame:

dim(dataset2)

head(dataset2)

summary(dataset2)

# Convert date back to a date object:

date = as.Date(dataset2$date, '%Y-%m-%d')

class(date)

head(date)

# All looks good, reassign the column:

dataset = dataset2

dataset$date = date

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset)

summary (dataset)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-
# COMMIT-PUSH THE UPDATED DATA FOLDER!


################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*