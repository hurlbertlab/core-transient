################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
# Start by opening the data formatting table (data_formatting_table.csv). To
# determine which dataset you should be working on, see the "format_priority"
# field. Choose the dataset with the highest format priority, but be sure to 
# check out the format_flag field to see the current status of the dataset.
# Flag codes are as follows:
  # 0 = not currently worked on
  # 1 = formatting complete
  # 2 = formatting in process
  # 3 = formatting halted, issue
  # 4 = data unavailable

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
# them, note that I've given a new name here "dataset1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[,-c(1, 2, 8, 9, 11,13, 14)]

# Let's change the name of the "record_record_date" column to simply "date":

names(dataset1)[8] = 'date'

# Explore, if everything looks okay, you're ready to move forward. If not, retrace your
# steps to look for and fix errors. 

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE: Fill in the value for Column H (R_nRecs)!

nrow(dataset)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# From the previous head commmand, we can see that sites are broken up into 
# (potentially) 5 fields. Find the metadata link in the data formatting table
# use that link to determine how sites are characterized.
#  -- If sampling is nested (e.g., site, block, treatment, plot, quad as in 
# this study), use each of the identifying fields and separate each field with
# an underscore.
# -- If sites are listed as lats and longs, use the finest available grain 
# and separate lat and long fields with an underscore.
# -- If the site definition is clear, make a new site column as necessary.

# Here, we will concatenate all of the potential fields that describe the 
# site:

site = paste(dataset1$site, dataset1$block, dataset1$treatment, 
             dataset1$plot, dataset1$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# For memory and cleaning purposes, removed the site object:

rm(site)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE: Fill in the value for Column I (R_nSites)!

length(unique(dataset2$site))

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do
# so, you need to look at the list of unique species very carefully. Avoid being
# too liberal in interpretation, if you notice an entry that MIGHT be a problem, 
# but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

levels(dataset2$species) 

# The first thing that I notice is that there are lower and upper case
# entries. Because R is case-sensitive, this will be coded as separate species.
# Modify this prior to continuing:

dataset2$species = toupper(dataset2$species)

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

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset the factor levels:

dataset3$species = factor(dataset3$species)

# Let's look at how the removal of bad species and altered the length of the dataset:

nrow(dataset2)

nrow(dataset3)

# Look at the head of the dataset to ensure everything is correct:

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE: Fill in the value for Column J (R_nSpecies)!

length(levels(dataset1$species))

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# For starters, let's change the date column to a true date:

date = strptime(dataset3$record_record_date, '%m/%d/%Y')

# A check on the structure lets you know that date field is now a date object:

class(date)

# Give a double-check, if everything looks okay replace the column:

head(dataset3$record_record_date)

head(date)

dataset4 = dataset3

dataset4$record_record_date = date

names(dataset4)[4] = 'date'
  
# Check the results:
  
head(dataset4)

# For memory and cleaning purposes, removed the date object:

rm(date)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE: Fill in the value for Column K (R_nTime)!

length(unique(dataset4$date))

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(dataset4)

# Subset to records > 0 (if applicable):

dataset5 = subset(dataset4, cover > 0) 

summary(dataset5)

# Remove NA's:

dataset6 = na.omit(dataset5)

# Make sure to write in the data summary table the type of observed count (here,
# it represents % cover)

# How does it look?

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE: Fill in the values in Columns K-M, 
# R_Mean_Individuals_perSiteYear, R_Min_Individuals_perSiteYear, and 
# R_Max_Individuals_perSiteYear. 

# If, as is the case here, the count is actually "cover", put "NA" in each of
# the columns. In Column Z (Notes_countFormat), enter "Count values represent 
# cover".

# Regardless of the type of data, be sure to include the removal of NA's or
# zeros.

# If the data were counts (which they are not here, this is only an example), 
# you would obtain the counts per site year summary statistics as follows:

tempCount = ddply(dataset6, .(species), summarize, tCount = sum(cover))

summary(dataset6)

str(dataset6)

dfx <- data.frame(
  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
  sex = sample(c("M", "F"), size = 29, replace = TRUE),
  age = runif(n = 29, min = 18, max = 54)
)

# Note the use of the '.' function to allow
# group and sex to be used without quoting
ddply(dfx, .(group, sex), summarize, mean = round(mean(age), 2))
ddply(dataset1, .(species), summarize, tCount = sum(cover))

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, let's add the datasetID:

dataset6$datasetID = rep(223,nrow(dataset1))

# Change date to a factor:

dataset6$date = factor(as.character(dataset1$date))

# Now make the compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(cover))


# Explore the data frame:

dim(dataset7)

head(dataset7)

summary(dataset7)

# Convert date back to a date object:

date = as.Date(dataset2$date, '%Y-%m-%d')

class(date)

head(date)

# All looks good, reassign the column:

dataset7$date = date

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-
# COMMIT-PUSH THE UPDATED DATA FOLDER!

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA_FORMATTING_TABLE  ----
#===============================================================================*

# Your last step in the process is to update the data formatting table. You must
# provide enough information to thoroughly explain any alterations to the final
# formatted dataframe!

# Start by filling in the information that pertains to the raw dataset prior
# to any formatting (field names that start with "R", columns F:L)
# 