################################################################################*
#  DATA CLEANING TEMPLATE
################################################################################*

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)
library(lubridate)

# Make sure you are in the core-transient directory

getwd()

# Source the functions file:

source('scripts/R-scripts/core-transient_functions.R')

# Get data:

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_208.csv')

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

#'data.frame':  48443 obs. of  9 variables:
#$ Sample_Date      : Factor w/ 765 levels "1989-05-24","1989-06-07",..: 765 765 765 765 765 765 765 765 765 765 ...
#$ Treatment        : Factor w/ 10 levels "CF","DF","SF",..: 2 10 10 10 2 10 2 10 10 3 ...
#$ Replicate_Station: Factor w/ 30 levels "1_1","1_2","1_3",..: 13 1 26 26 14 11 15 11 11 1 ...
#$ Species          : Factor w/ 22 levels "Adalia bipunctata",..: 18 3 10 9 18 10 18 3 6 18 ...
#$ Family           : Factor w/ 8 levels "","Cantharidae",..: 7 2 4 4 7 4 7 2 4 7 ...
#$ Order            : Factor w/ 5 levels "","Coleoptera",..: 4 2 2 2 4 2 4 2 2 4 ...
#$ Adults           : int  2 1 2 1 4 1 6 1 1 1 ...
#$ location_utm     : Factor w/ 255 levels "(631819.64,4697018.97)",..: 38 85 42 42 39 170 40 170 170 17 ...
#$ Year             : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...

# View first 6 rows of the dataset:

head(dataset)

# Here, we can see that there are some fields that we won't use. Let's remove
# them, note that I've given a new name here "dataset1", this is to ensure that
# we don't have to go back to square 1 if we've miscoded anything.

names(dataset)

unnecessaryFields = c("Family", "Order", "location_utm")

dataset1 = dataset[, !names(dataset) %in% unnecessaryFields]

head(dataset1)

# Rename fields using standardized names where appropriate
names(dataset1)[names(dataset1) %in% c('Species', 'Adults', 'Year')] = 
  c('species', 'count', 'date')

# Because all (and only) the fields we want are present, we can re-assign dataset1:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# View summary of fields in the dataset:

summary(dataset)

# Reminder of the dataset:

head(dataset)

# I found a description of the spatial data collection available here:
# https://knb.ecoinformatics.org/#view/doi:10.5063/AA/mcolunga60.3.2

# My reading of this is that the entire study site was divided into 6 blocks
# ('Replicate', I think) and in each block there were 7 different 1-ha plots 
# each receiving a different cropping system treatment ('Treatment', which 
# in addition to 'T1' thru 'T7' also includes 'DF', 'CF', and 'SF' which I am 
# unclear on). Within each plot there are 5 permanent trap locations. 

# The spatial grain of analysis probably shouldn't be an individual sticky
# trap location. Possibly the grain could be the plot (a set of 5 trap locations)
# which would be the concatenation of Treatment and Replicate fields, or it 
# could be the entire block of 7 different treatments (i.e., just the Replicate
# field).

# As I can't find more detailed info at present, it seems that the 'DF', 'CF',
# and 'SF' treatments should be excluded.
dataset1 = dataset[!dataset$Treatment %in% c('DF', 'CF', 'SF'), ]

# Here, we use the 1 ha treatment plots each consisting of 5 traps locations)

################################################################################
# MODIFY HERE ONCE RAW DATA HAS BEEN REVERTED BACK TO SEPARATE "Replicate" AND
# "Station" FIELDS
dataset1$site = paste(substr(dataset1$Replicate_Station,1,1), 
                      dataset1$Treatment, sep = "_")

dataset1$Station = substr(dataset1$Replicate_Station,3,3)

################################################################################



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

# Now explore the listed species themselves. To do so, you should go back to study's 
# metadata. 

bad_sp = c('something else')

dataset1 = dataset[!dataset$species %in% bad_sp,]

dataset1$species = factor(dataset1$species)

# Let's look at how the removal of bad species altered the length of the dataset:

nrow(dataset) #43314

nrow(dataset1) #43167

# Look at the head of the dataset to ensure everything is correct:

head(dataset1)

# Having checked through the results, we can now reassign the dataset:

dataset = dataset1

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Here, we need to extract the sampling dates. For most studies, we will be 
# calculating temporal occupancy based on annual snapshots of our communities.
# For communities that are sampled once per year, this is fairly straightforward
# but for sites that are sampled multiple times per year, we want to ensure
# that both across sites and across years, that level of sampling effort does
# not vary wildly.

# For starters, let's make sure that the number of sampling events per site
# within a year does not change systematically across years.

# The list of unique Sample_Date x site combinations
uniqSiteDate = unique(dataset[, c('Sample_Date', 'date', 'site')])

# Count up the number of Sample_Dates per site-year and remove 0's
samplingPerSiteYear = data.frame(table(uniqSiteDate[, c('date', 'site')]))
samplingPerSiteYear = samplingPerSiteYear[samplingPerSiteYear$Freq != 0, ]

# Calculate the mean sampling per site in each year
meanSamplingPerYear = aggregate(samplingPerSiteYear$Freq, 
                                by = list(samplingPerSiteYear$date), mean)

# Plot in two different ways
plot(as.numeric(as.character(samplingPerSiteYear$date)), 
    samplingPerSiteYear$Freq, xlab = 'Year', 
    ylab = 'Mean sampling events per site')
points(as.numeric(as.character(meanSamplingPerYear$Group.1)),
       meanSamplingPerYear$x, type = 'l', lwd = 3)

hist(samplingPerSiteYear$Freq)

# You can see there is a bit of variation over time, with higher sampling
# in 2001 and 2002, as well as 1990 and 2012. This could bias results 
# towards more transient species (i.e., presumably there are species 
# that will only be observed in high sampling years).

# This means we should choose some standardized number of sampling events
# with which to characterize the assemblage in any given year. Sites that
# do not have sufficient sampling will not be characterized for that year.
# Sites with more sampling events will have the standardized number of
# events chosen at random, while ensuring stratification across the
# time of year over which surveys are conducted. (Wouldn't want to 
# randomly choose all the surveys in June in one year, and all the surveys
# in August in another.)

# So first, let's get a sense of that time of year over which surveys are
# conducted. We'll convert dates to Julian Day for convenience.
dataset$sdate = as.Date(dataset$Sample_Date, "%Y-%m-%d")
dataset$julianDay = yday(dataset$sdate)
uniqSiteDay = unique(dataset[, c('julianDay', 'site', 'date')])
daycount = data.frame(table(uniqSiteDay$julianDay))

plot(as.numeric(as.character(daycount$Var1)), daycount$Freq, xlab = "Julian Day", 
     ylab = "Sampling Events", type = 'l')

# We might get a slightly clearer picture if we count the number of sampling
# events on a weekly basis throughout the summer instead of a daily basis.
uniqSiteDay$julianWeek = 7*floor(uniqSiteDay$julianDay/7)
weekcount = data.frame(table(uniqSiteDay$julianWeek))

plot(as.numeric(as.character(weekcount$Var1)), weekcount$Freq, xlab = "Julian Day", 
     ylab = "Sampling Events per week", type = 'l')
abline(v = c(164, 216), col = 'red')

# From these graphs, we need to make a decision on the window over which 
# sampling over all sites is fairly consistent. This requires a somewhat
# arbitrary decision, but in this case let's go with Julian days 155 to 227,
# or May 30 to August 15.

begDay = 164
endDay = 216

dataset1 = dataset[dataset$julianDay >= begDay & dataset$julianDay <= endDay,]

# Now let's revisit how frequently sites are visited per year given
# this more restricted window.
uniqSiteDate1 = unique(dataset1[, c('Sample_Date', 'date', 'site')])
samplingPerSiteYear1 = data.frame(table(uniqSiteDate1[, c('date', 'site')]))
samplingPerSiteYear1 = samplingPerSiteYear1[samplingPerSiteYear1$Freq != 0, ]
hist(samplingPerSiteYear1$Freq, xlab = "Sampling events per year", main = "")

# We can quickly check what fraction of site-years would meet different
# required levels of sampling from 1:10

sapply(1:10, function(x) 
  round(sum(samplingPerSiteYear1$Freq > x)/nrow(samplingPerSiteYear1), 2))

#  [1] 0.96 0.92 0.85 0.65 0.46 0.28 0.11 0.00 0.00 0.00
# This tells us that at a minimum of 4 samples per year, we would still be able
# to use 65% of all site-years.

# We can also do a quick check to see how many years of data meet
# this criterion at each site. First, you can see that without any criterion
# sites were sampled for 25 years (1989-2013).
table(samplingPerSiteYear1$site)

table(samplingPerSiteYear1$site[samplingPerSiteYear1$Freq >= 4])
table(samplingPerSiteYear1$site[samplingPerSiteYear1$Freq >= 5])

# We see that requiring 5 samples per year drops a number of sites below
# 15 usable years, while all sites have at least 16 usable years using
# a minimum of 4 samples per year. So let's specify our number of
# subannual samples for standardization:

num_subsamples = 4

# This means we want to divide up the time window established above
# into 4 periods. They will have midpoints as follows

periodLength = (endDay - begDay + 1)/num_subsamples

period_midpoints = round(begDay + periodLength/2 + 
                           0:(num_subsamples-1)*periodLength, 0)

# Now assign the Julian day of each sampling event to one of these
# sampling periods.
dataset1$samplingPeriod = round(periodLength*floor((dataset1$julianDay - 
                                 begDay)/periodLength) + period_midpoints[1], 0)

# And now, we would like to reduce the data down so that for any 
# site, only one sampling event per sampling period is represented.

# Let's use a small subset (site 6_T7 in 2013) which we know has 11 samples.
sub = subset(dataset1, site=='6_T7' & date==2013)

# A function that samples n rows of a dataframe at random
sample.df = function(dataset, n) dataset[sample(nrow(dataset), n), ]

sampledData = c()
for (p in period_midpoints) {
  temp = subset(sub, samplingPeriod == p)
  tempsample = sample.df(temp, 1)
  sampledData = rbind(sampledData, tempsample)
}



uniqSitePeriod = unique(dataset1[, c('site', 'date', 'samplingPeriod')])
periodsPerSiteYear = data.frame(table(uniqSitePeriod[,c('site','date')]))













#


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

dataset1$datasetID = rep(208,nrow(dataset1))

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
# If you had to set the formatting script aside and opened this in a new 
# session, source the script and load required libraries and dataset:

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_223.csv")

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
# We have now formatted the dataset to the finest possible spatial and temporal
# grain, removed bad species, and added the dataset ID. It's now to make some
# scale decisions and determine the proportional occupancies.

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? Return to the metadata to
# see if there's any clues.

# If the spatial grain of analysis is larger than the finest grain of the data,
# then we will have to make sure that each "site" is being defined based on 
# a constant number of subplots. How many subplots will depend on the nature
# of the sampling design and how regularly different subplots were sampled.
# If there is variation in the number of subplots sampled across years, this
# could bias our estimation of the relative fraction of core vs transient species.

# As a rule, we will only include a site-year (that is, a snapshot of a community
# at a particular site in a particular year) if that site-year is represented by
# a large number of subplots relative to distribution of subplot sampling. 

# The list of unique Year x subplot combinations. In this case, "site" refers
# to the spatial grain we have deciding on conducting analyses at, while 
# "Station" is the finer spatial grain within sites.
uniqSiteDateStations = unique(dataset1[, c('date', 'site','Station')])

# Count up the number of Sample_Dates per site-year and remove 0's
samplingPerSiteYear = data.frame(table(uniqSiteDateStations[, c('date', 'site')]))
samplingPerSiteYear = samplingPerSiteYear[samplingPerSiteYear$Freq != 0, ]
# For dataset 208, every site ('Replicate_Treatment') has 5 sticky trap Stations





# How many unique sites are there? Does this jive with what you expect
# based on the metadata?

length(unique(site))

# For dataset 208, this is 42 which sounds right given 6 blocks x 7 treatments per block

# Do some quality control by comparing the site fields in the dataset with the 
# new vector of sites:

head(site)

# All looks correct, so replace the site column in the dataset (as a factor) 
# and remove the unnecessary fields, start by renaming the dataset in case 
# you make a mistake:

dataset1$site = factor(site)

dataset1 = dataset1[, !names(dataset1) %in% c("Treatment", "Replicate_Station")]

# Check the new dataset (are the columns as they should be?):

head(dataset1)



# How many sites are there?

length(unique(dataset$site))

# How many time and species records are there per site?

siteTable = ddply(dataset, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

# We see that each of the sites was sampled with equivalent, and adequate,
# time samples (>4) but that at least some sites have species richness 
# below the cut-off value of 10. Perhaps too many sites of with low sr?

# Let's sort and have a look at the first few rows:

head(siteTable[order(siteTable$nSp),],20)

# All 1's! How many sites have less than 10 species?

nrow(siteTable)
nrow(subset(siteTable, nSp < 10))

# That's almost a third of the sites! This is a clue that the 
# smallest spatial sampling grain (quadrat) is too fine.

# Let's try concatenating all but the quad field and explore the output. 
# We start by splitting site:

site = read.table(text = as.character(dataset$site), sep ='_')

head(site)

site1 = do.call('paste', c(site[,1:4],sep = '_'))

head(site1)

length(site1)

# How have we changed the number of sites?

length(unique(dataset$site))

length(unique(site1))

# We've reduced the number of sites to 28! How does the richness look
# for this new spatial sampling grain?

dataset1 = dataset

dataset1$site = site1

siteTable = ddply(dataset1, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

head(siteTable[order(siteTable$nSp),],10)

# For all but the first site (and perhaps the second), the species richness
# is adequate.Change the dataset site column to this one:

dataset$site = dataset1$site

# Now let's remove the sites with inadequate sample sites:

badSites = subset(siteSummaryFun(dataset), spRich < 10 | nTime < 5)$site

dataset1 = dataset[!dataset$site %in% badSites,]

# Summarize the dataset to the new spatial grain:

dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

head(dataset2)

dim(dataset2)

summary(dataset2)

# All looks good, rename dataset:

dataset = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY SPATIAL GRAIN DECISIONS!

# Note: In many instances, site definition will be spatially explicit (e.g., 
# lats and longs). When this is the case, we may need to summarize the data to
# a courser precision (few decimal places). We can do so by using the 
# "round_any" function in Hadley Wickham's plyr package, specifying "floor" 
# as the rounding function.


#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Because it's often considerably more straightforward, we'll start with the
# temporal data. For this, go to the metadata of the study. There, we see
# that data are collected in two seasons, spring and fall. We want to change
# these to a decimal year, which are simply grouped time samples (the actual 
# values don't really matter much).









# Extract year values:

year = as.numeric(format(dataset$date, '%Y'))

head(year)

summary(year)

# Extract month values (if applicable):

month = as.numeric(format(dataset$date, '%m'))

head(month)

summary(month)

unique(month)

# Make "season" values:

season = ifelse(month < 7, .25, .75)

unique(season)

# Add year and season:

yearSeason = year + season

head(yearSeason)

summary(yearSeason)

# Modify date column to now represent the decimal year:

dataset1 = dataset

dataset1$date = yearSeason

head(dataset1)

# Change the column name to year:

names(dataset1)

names(dataset1)[3] = 'year'

# Summarize the dataset to the new temporal grain:

dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

# Explore:

dim(dataset1)
dim(dataset2)

head(dataset1)
head(dataset2)

str(dataset2)

# All looks okay, rename as dataset:

dataset = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY TEMPORAL GRAIN DECISIONS!

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# And make our proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH propOcc!

# And make and write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_223.csv', row.names = F)

# Note: Both the submodule and core-transient folder need to be pushed to, 
# in git bash:

# cd data
# git add formatted_datasets/dataset_208.csv
# git commit -m "added formatted dataset"
# git push
# cd ..
# git add data
# git commit -m "updated submodule with formatted dataset 208"
# git push

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

# !!!At this point, go to the data source table and provide:
#   -central lat and lon (if available, if so, LatLonFLAG = 0, if you couldn't do
#    it, add a flag of 1)
#   -spatial_grain columns (T through W)
#   -nRecs, nSites, nTime, nSpecies
#   -temporal_grain columns (AH to AK)
#   -Start and end year
#   -Any necessary notes
#   -flag any issues and put issue on github
#   -git-add-commit-push data_source_table.csv

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$year))

length(unique(dataset$species))

