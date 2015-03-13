################################################################################*
#  DATA FORMATTING TEMPLATE
################################################################################*
# Start by opening the data formatting table (data_formatting_table.csv). To determine which dataset you should be working on, see the "format_priority" field. Choose the dataset with the highest format priority, but be sure to check out the format_flag field to see the current status of the dataset.

# Flag codes are as follows:
  # 0 = not currently worked on
  # 1 = formatting complete
  # 2 = formatting in process
  # 3 = formatting halted, issue
  # 4 = data unavailable

# NOTE: All changes to the data formatting table will be done in R! Do not make changes directly to this table, this will create conflicting versions.

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Source the functions file:

getwd()

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_223.csv')

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Here, you are predominantly interested in getting to know the dataset, and determine what the fields represent and  which fields are relavent.

# View field names:

names(dataset)

# View how many records and fields:

dim(dataset)

# View the structure of the dataset:

str(dataset)

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove them, note that I've given a new name here "dataset1", this is to ensure that we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

dataset1 = dataset[,-c(1, 2, 8, 9, 11,13, 14)]

# Let's change the name of the "record_record_date" column to simply "date":

names(dataset1)[8] = 'date'

# Explore, if everything looks okay, you're ready to move forward. If not, retrace your steps to look for and fix errors. 

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! Are the sites defined by latitude and longitude? Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(223, 'LatLong_sites','Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# From the previous head commmand, we can see that sites are broken up into (potentially) 5 fields. Find the metadata link in the data formatting table use that link to determine how sites are characterized.

#  -- If sampling is nested (e.g., site, block, treatment, plot, quad as in this study), use each of the identifying fields and separate each field with an underscore. For nested samples be sure the order of concatenated columns goes from coarser to finer scales (e.g. "km_m_cm")

# -- If sites are listed as lats and longs, use the finest available grain and separate lat and long fields with an underscore.

# -- If the site definition is clear, make a new site column as necessary.

# Here, we will concatenate all of the potential fields that describe the site:

site = paste(dataset1$site, dataset1$block, dataset1$treatment, 
             dataset1$plot, dataset1$quad, sep = '_')

# Do some quality control by comparing the site fields in the dataset with the new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) and remove the unnecessary fields, start by renaming the dataset to dataset2:

dataset2 = dataset1

dataset2$site = factor(site)

dataset2 = dataset2[,-c(2:5)]

# Check the new dataset (are the columns as they should be?):

head(dataset2)

# For memory and cleaning purposes, removed the site object:

rm(site)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded (i.e. if the field was concatenated such as this one, it was coded as "site_block_treatment_plot_quad"). Alternatively, if the site were concatenated from latitude and longitude fields, the encoding would be "lat_long". 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(223, 'Raw_siteUnit','site_block_treatment_plot_quad')


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(223, 'spatial_scale_variable','Y')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(223, 'Notes_siteFormat','site fields concatenated. metadata suggests site-block-treatment-plot-quad describes the order of nested sites from small to large.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

# Look at the individual species present:

levels(dataset2$species) 

# The first thing that I notice is that there are lower and upper case entries. Because R is case-sensitive, this will be coded as separate species. Modify this prior to continuing:

dataset2$species = toupper(dataset2$species)

# Now explore the listed species themselves. To do so, you should go back to study's metadata. A quick look at the metadata is not informative, unfortunately. Because of this, you should really stop here and post an issue on GitHub. With some more thorough digging, however, I've found the names represent "Kartez codes". Several species can be removed (double-checked with USDA plant codes at plants.usda.gov and another Sevilleta study (dataset 254) that provides species names for some codes). Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf

bad_sp = c('','NONE','UK1','UKFO1','UNK1','UNK2','UNK3','LAMIA', 'UNGR1','CACT1','UNK','NONE','UNK2','UNK3', 'UNK1','FORB7', 'MISSING', '-888', 'DEAD','ERRO2', 'FORB1','FSEED', 'GSEED', 'MOSQ', 'SEED','SEEDS1','SEEDS2', 'SEFLF','SESPM','SPOR1')

dataset3 = dataset2[!dataset2$species %in% bad_sp,]

# Reset the factor levels:

dataset3$species = factor(dataset3$species)

# Let's look at how the removal of bad species and altered the length of the dataset:

nrow(dataset2)

nrow(dataset3)

# Look at the head of the dataset to ensure everything is correct:

head(dataset3)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(223, 'Notes_spFormat', 'several species removed. Metadata was relatively uninformative regarding what constitutes a true species sample for this study. Exploration of metadata from associated Sevilleta studies were more informative regarding which species needed to be removed. Species names are predominantly provided as Kartez codes, but not always. See: http://sev.lternet.edu/data/sev-212/5048. Some codes were identified with this pdf from White Sands: https://nhnm.unm.edu/sites/default/files/nonsensitive/publications/nhnm/U00MUL02NMUS.pdf')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(dataset3)

# Subset to records > 0 (if applicable):

dataset4 = subset(dataset3, cover > 0) 

summary(dataset4)

# Remove NA's:

dataset5 = na.omit(dataset4)

# Make sure to write in the data summary table the type of observed count (here, it represents % cover)

# How does it look?

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_countFormat. Provide a complete description of how count data formatted.  Regardless of the type of data, be sure to include the removal any NA's or zeros in the notes field.

dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(223, 'countFormat','cover')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(223, 'Notes_countFormat','Data represents cover. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for errors, and remove records that cant be used for our purposes.

# First, lets add the datasetID:

dataset5$datasetID = 223
  
# Now make the compiled dataframe:

dataset6 = ddply(dataset5,.(datasetID, site, date, species),
                 summarize, count = max(cover))

# Explore the data frame:

dim(dataset6)

head(dataset6)

summary(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. 

# For starters, let's change the date column to a true date:

date = as.POSIXct(strptime(dataset6$date, '%m/%d/%Y'))

# A check on the structure lets you know that date field is now a date object:

class(date)

# Give a double-check, if everything looks okay replace the column:

head(dataset6$date)

head(date)

dataset7 = dataset6

dataset7$date = date

# Check the results:

head(dataset7)

# For memory and cleaning purposes, removed the date object:

rm(date)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(223, 'Notes_timeFormat', 'temporal data provided as dates. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(223, 'subannualTgrain', 'Y')

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE  ----
#===============================================================================*
# Your goal is to now fill in the remainder of the data formatting table.

dataFormattingTableUpdate(223)

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

