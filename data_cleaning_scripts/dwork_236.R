# Dataset 236: Powdermill small mammals

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d236 = read.csv(file.path(in_dir,'dataset_236.csv'))

# Goal is to change monthly sample to some decimal of year (breaking into quarters):

# Month is embedded within a date string (YYYYMM), extract month:

d = as.numeric(substr(as.character(d236$mo), 5,6))

# Change month to season (wint = Dec, Jan, Feb, spr = Mar, Apr, May, sum  = Jun, Jul, Aug, etc.)

d1 = .1* ifelse(d >= 3 & d <=5, 1, 
                ifelse(d >= 6 & d <= 8, 2,
                       ifelse(d >= 9 & d <=11, 3, 4)))

# Extract year from the date column:

y = as.numeric(substr(as.character(d236$mo), 1,4))

# Add the decimal season to the year column:

d236$year =y + d1

# Create a "site" column, the sites are the 20 experimental grids

site = paste('d236_',d236$gr, sep = '')

# Make initial frame (necessary columns, not summarized):

df1 = data.frame(site, d236$sp, d236$year, d236$mo)
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

df4$datasetID = rep(236, length(df4[,1]))

# Rearrange the columns"

d236 = df4[,c(5,1:4)]

# Write to file:

write.csv(d236, file.path(out_dir,'dataset_234.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
