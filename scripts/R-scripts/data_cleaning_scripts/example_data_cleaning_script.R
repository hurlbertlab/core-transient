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
# Sevilleta confirmed this). What if we concatenated all of the site columns?
# Use paste to concatenate:

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

# Let's try concatenating all but the quad field and explore the output:

site = paste(d$site, d$block, d$treatment, d$plot, sep = '')

length(unique(site))

siteTable = ddply(data.frame(site), .(site), nrow)

head(siteTable[order(siteTable$V1),],10)

summary(siteTable)

# For all but the first site (and perhaps the second), these sample sizes are 
# adequate. Add to reduced dataframe:

d1 = d[,-c(2:5)]

d1$site = site

head(d1)

# Now let's remove the site with the very low sample size:

head(siteTable[order(siteTable$V1),],10)

d1 = d1[!d1$site %in% 'C3C1',]

head(d1)

d = d1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Here, your primary goal is to ensure that all of your species are valid. To do
# so, you need to look at the list of unique species very carefully. Avoid being
# too liberal in interpretation, if you notice an entry that MIGHT be a problem, 
# but you can't say with certainty, create an issue on GitHub.

sp = d$species

levels(sp) # Note: You can also use unique(sp) here.

# There first thing that I notice is that there are lower and upper case
# entries. Because R is case-sensitive, this will be coded as separate species.
# Modify this prior to continuing:

d$species = toupper(d$species)

# Let's explore whether there was a difference:

length(unique(d$species))

length(unique(sp))

# We see that almost 70 species were the result of upper and lower case!
# Make a new species vector (factor ensures that it is coded as a factor
# rather than character and removes any unused levels) 
# and continue exploring:

sp = factor(d$species)

levels(sp)

# There are a number of records that can be removed. There are actually more than
# this in the example dataset, this should be posted as an issue on GitHub, but
# we will continue with the example:

bad_sp = c('', 'DEAD','SEED','SEED1','SEED2')

d1 = d[!d$species %in% bad_sp,]

head(d1)

summary(d1)

# Having checked through the results, we can now reassign the dataset as d:

d = d1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Next, we need to explore the count records. A good first pass is to remove 
# zero counts and NA's:

summary(d)

# Subset to records > 0

d1 = d[d$cover>0,]

summary(d1)

# Remove NA's:

d = na.omit(d1)

# Let's change the cover column to count. Make sure to write in the data summary
# table the type of observed count.

names(d)[4] = 'count'

head(d)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to modify the dates of sampling to decimal years. To do so, we 
# we need to be aware of the temporal grain of the analysis.

# Let's look at how the seasons are distributed by extracting just the 
# season (not year data):

head(d)

season = str_sub(d$season, end = -5)

levels(factor(season))

# Okay we can see that the sampling is divided into fall and spring. Let's 
# just turn those into decimal years:

season = ifelse(season == 'SPRING',.25, .75)

summary(season)

# Now, let's extract year from the date (we could easily do this using
# the str_sub method as well, but this is more universal).
# First, make the date into an R date object:

class(d$record_record_date)

date = strptime(d$record_record_date, '%m/%d/%Y')

class(date)

head(date)

# It worked! Now extract year:

year = as.numeric(format(date, '%Y'))

# Add the decimal year and year vectors:

d$year = year + season 

head(d)

# Now let's clean up by removing the other date columns:

d1 = d[,-c(2,5)]

head(d1)

summary(d1)

# A couple of years are listed as NA. Let's remove them:

d1 = na.omit(d1)

summary(d1)

# Everything looks good, so let's call it d again

d = d1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Now we will make the final formatted dataset, add a datasetID field, check for
# errors, and remove records that can't be used for our purposes.

# First, lets add the datasetID:

d$datasetID = rep(223,nrow(d))

d1 = ddply(d,.(datasetID, site, year, species), summarize, count = max(count))

head(d1)
dim(d1)
summary(d1)

# Let's change site to a factor, so we can explore it further:

d1$site = factor(d1$site)

summary(d1)

# Now let's check and make sure each site has at least 10 species:

t1 = ddply(d1, .(site), summarize,richness = length(unique(species)))

head(t1[order(t1$richness),])

# All of our sites pass the richness test!

# Now let's check whether each site has at least 5 time samples.

t1 = ddply(d1, .(site), summarize, nT = length(unique(factor(year))))

head(t1[order(t1$nT),])

# Three sites need to be removed!

bad_sites = c('G1C1','G1R4','G2C2')

d = d1[!d1$site %in% bad_sites,]

# Look at summaries again:

head(d)

summary(d)

# Everything looks great!

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- WRITE DATA FRAME TO FORMATTED DATASETS ----
#===============================================================================*

write.csv(d, "formatted_datasets/dataset_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH BOTH YOUR COMPLETED SCRIPT AND THE NEW FORMATTED DATASET!

