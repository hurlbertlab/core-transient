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

df1 = data.frame(site, d234$species, d234$year)
  colnames(df1)[2:3] = c('species','year')

-