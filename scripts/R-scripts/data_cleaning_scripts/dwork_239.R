# Dataset 239: Phytoplankton data from cruises. 

# Add libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# These datasets include spatial sample data (t1) and collection data (t3)

d239t1 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table1.csv'))
d239t3 = read.csv(file.path(in_dir,'/dataset_239RAW/dataset_239_table3.csv'))

# ************************************
# ----- Spatial sampling data ----
# ************************************

# Extract the columns of interest:

t1 = d239t1[,c(1,3,7:8)]

# Remove NA's

t1 = na.omit(t1)

# Create a spatial points dataframe with lat lon 

t1sp = SpatialPoints(data.frame(t1$Lon, t1$Lat),  proj4string = CRS('+proj=longlat +datum=WGS84'))

# Create an extent object from the point file (stretching the boundaries by 1 degree):

t1.extent = extent(extent(t1sp)@xmin-10,
                   extent(t1sp)@xmax+10,
                   extent(t1sp)@ymin-10,
                   extent(t1sp)@ymax+10)

# Create an empty raster from the extent object with a resolution of one degree:

r = raster(t1.extent, resolution = 8, crs ='+proj=longlat +datum=WGS84')

# Assign a unique value to each cell:

r = setValues(r, 1:ncell(r))

# Extract the cell assignment to the site table:

t1$site = extract(r, t1sp)

# Testing:

head(t1)

t1[t1$site == 94,]

# It seems there is paired shallow and deep samples for each lat-lon, how to deal with?
# Trade-off with resolution? Think on this ...

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
