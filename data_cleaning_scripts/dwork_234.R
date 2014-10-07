# Dataset 234: Powdermill small mammals

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

d234 = read.csv(file.path(in_dir,'dataset_234.csv'))

# Goal is to change monthly sample to some decimal of year (breaking into quarters):

# Month is embedded within a date string (YYYYMMDD), extract month:

d = as.numeric(substr(as.character(d234$date), 5,6))

# Change month to season (wint = Dec, Jan, Feb, spr = Mar, Apr, May, sum  = Jun, Jul, Aug, etc.)

d1 = .1* ifelse(d >= 3 & d <=5, 1, 
            ifelse(d >= 6 & d <= 8, 2,
            ifelse(d >= 9 & d <=11, 3, 4)))

# Add the decimal season to the year column:

d234$year = d234$year + d1

# Create a "site" column (just one site)

site = rep('d234_pm',length(d234[,1]))

# Make initial frame (necessary columns, not summarized):

df1 = data.frame(site, d234$species, d234$year, d234$date)
  colnames(df1)[2:4] = c('species','year','date')

# Some species are blank (47 of them) and others are "?" (1) ... remove:

df1 = df1[df1$species!='' & df1$species != '?',]

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

df4$datasetID = rep(234, length(df4[,1]))

# Rearrange the columns"

d234 = df4[,c(5,1:4)]

# Write to file:

write.csv(df234, file.path(out_dir,'dataset_234.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())
