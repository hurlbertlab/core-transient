# D208, extracting site information.

# Get data:

d208 = read.csv('dataset_208.csv')

d208 = d208[!is.na(d208$station)&!is.na(d208$treatment),]

# Remove non-treatments (CD, DF, and SF ... these also don't have UTM's):

d208 = d208[d208[,3]!= 'CF' & d208[,3]!='DF'&d208[,3]!='SF',]

# Concatenate treatment and replicate columns for siteID:

d208$siteID = factor(paste(d208$treatment, d208$replicate, sep = '_'))

# Some summary info of the dataset (for the data source table):

summary(d208$siteID)

N_samples = length(d208[,1])
N_unique_temporal_samples = length(d208$year)
species_richness = length(unique(d208$species))

# Retain just site and coordinate columns:

d208a = d208[,c(12,10)]

# Remove duplicates:

d208a = unique(d208a)

# Remove "(" and ")" characters from the coordinates file:

utm1 = gsub('\\(','',d208a$location_utm)
utm2 = gsub('\\)','',utm1)

utm.x = numeric()
utm.y = numeric()
for(i in 1:length(d208a$siteID)){
  utm.x[i] = as.numeric(strsplit(utm2, split=',')[[i]][1])
  utm.y[i] = as.numeric(strsplit(utm2, split=',')[[i]][2])
}

d208b = data.frame(d208a[,1],utm.x, utm.y)

siteID = sort(unique(d208b[,1]))
utm.x = as.vector(tapply(d208b$utm.x, d208b[,1],mean))
utm.y = as.vector(tapply(d208b$utm.y, d208b[,1],mean))

d208.site = data.frame(siteID, utm.x, utm.y)

# Convert utm to longlat:

library(rgdal)
  sp = SpatialPoints(cbind(d208.site$utm.x, d208.site$utm.y))
    proj4string = CRS('+proj=utm +zone=16')
  sp2 = spTransform(sp, CRS('+proj=longlat'))

