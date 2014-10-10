# Dataset 239: Phytoplankton data from cruises. 

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# These datasets include spatial sample data (t1) and collection data (t3)

d239t1 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table1.csv'))
d239t3 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table3.csv'))

# Change date to year:

year = as.numeric(format(as.Date(d239t1$Date, format = '%d-%m-%Y'), '%Y'))
d239t1[,3] = year

# Function to set the resolution for the creation of spatial sites:

res.set = function(resolution){
  Lat = round_any(d239t1$Lat, resolution)
  Lon = round_any(d239t1$Lon, resolution)
  d239t1$site = paste(Lat,Lon, sep = '')
  d239t1
}

# Testing mechanism ... in order to satisfy the # of years condition for the analysis
# we need to ensure that a given resolution yields a certain number of years (our limit is 5 years):


test.fun = function(resolution){
  t = res.set(resolution)
  num.years = function(x) length(unique(x))
  t1 = aggregate(Date~site, data = t, num.years)
  t1[t1[,2]>4,]
}

test.fun(1)

# If we explore a number of resolutions above, we can see that there is only one site in which
# the number of years is 5 or more for an incredible range of resolutions. This means that, for 
# our purpose, only site 50-4, at one degree resolution can truly be used.

# Subset to only the spatial location with an adequate number of temporal samples.

d239t1 = res.set(1)

d239t1 = d239t1[d239t1$site == '50-4',]

# Subset the community data to only records from that location:

d239t3b = d239t3[d239t3$X  %in% d239t1$SampleID == T,]

# Note: despite previous concerns of samples from varying depths, it appears that all samples
# within this subset are from surface waters.


###------REFERENCE SCRIPT BELOW--------------
# Goal is to change monthly sample to some decimal of year (breaking into quarters):

# Month is embedded within a date string (YYYYMM), extract month:

d = as.numeric(substr(as.character(d239$mo), 5,6))

# Change month to season (wint = Dec, Jan, Feb, spr = Mar, Apr, May, sum  = Jun, Jul, Aug, etc.)

d1 = .1* ifelse(d >= 3 & d <=5, 1, 
                ifelse(d >= 6 & d <= 8, 2,
                       ifelse(d >= 9 & d <=11, 3, 4)))

# Extract year from the date column:

y = as.numeric(substr(as.character(d239$mo), 1,4))

# Add the decimal season to the year column:

d239$year =y + d1

# Create a "site" column, the sites are the 20 experimental grids

site = paste('d239_',d239$gr, sep = '')

# Make initial frame (necessary columns, not summarized):

df1 = data.frame(site, d239$sp, d239$year, d239$mo)
colnames(df1)[2:4] = c('species','year','date')

# Create a data frame of the count of individuals for a given sampling event:

df2 = ddply(df1, .(site, year, species, date), 
            summarise, count = length(species))

# Create a data frame of the maximum count of individuals 
# for a given sampling event within a season.

df3 = ddply(df2,.(site,year,species),
            summarise, count = max(count))

# Arrange the fields in the same order as other datasets:

df4 = data.frame(df3[,1],df3[,3],df3[,2],df3[,4])
names(df4) = c('site','species','year','count')

# Add a dataset ID column for matching with metadata

df4$datasetID = rep(239, length(df4[,1]))

# Rearrange the columns"

d239 = df4[,c(5,1:4)]

# Write to file:

write.csv(d239, file.path(out_dir,'dataset_239.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
