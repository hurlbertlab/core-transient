# Formatting Dataset 200 OBIS Inverts

# Get Data

getwd()
setwd('C:/Users/auriemma/core-transient/')
d = read.csv('raw_datasets/dataset_200.csv')
dim(d)
head(d)
str(d)

#===================================================================
# SPECIES data

# Create species vector
sp = d$sname
length(unique(sp))

# Capitalize all to remove letter case error
d$species = toupper(d$sname)
head(d$species)
head(d)
  # Compare with original
length(unique(sp))
length(unique(d$species))
  # No difference, no error in capitalization, so remove old species column
d = d[,-c(5)]
head(d)

# Search for species to remove
unique(d$species)

  #Species to remove
remove_sp = c('UNIDENTIFIED FISH')
d1 = d[!d$species%in%remove_sp,]
head(d1)
dim(d1)
dim(d)

# Species listed as common names, low level taxonomy, or others
# Did not remove any of these listed, but make note of them in markdown file

  # Species listed as common name:
  common = c('EEL UNCL','SHRIMP UNCL','LONGFIN HAKE','THORNY SKATE','LUMPFISH SNAILFISH UNCL','ROUGH SCAD','BLUE HAKE',
             'BUTTERFISH','BOBTAIL UNCL','BARNDOOR SKATE','WITCH FLOUNDER','CRUSTACEA SHRIMP','GOOSEFISH','CRAB BRACHYURAN UNCL',
             'NORTHERN STONE CRAB','JELLYFISH UNCL')
  
  # Listed unspecific taxonomy
  tax = c('CEPHALOPODA','RAJIFORMES','VAMPYROMORPHIDA','OCTOPODA','ANGUILLIFORMES','PLEURONECTIFORMES','GASTROPODA','STOMATOPODA',
          'LOPHIIFORMES','MOLLUSCA')
          
  #Several FAMILY names listed
  
  # Others in question
  other = c('CANCER BOREALIS MALE','HOMARUS AMERICANUS FEMALE','LOLIGO PEALEII EGG MOPS','HOMARUS AMERICANUS MALE',
            'GALATHEID UNCL','CANCER BOREALIS FEMALE','ILLEX ILLECEBROSUS EGG MOPS')

d= d1
head(d)        
  
#===================================================================
# COUNT data

length(unique(d$count))
summary(d)
class(d$count)

# Change data to numeric
d$count = as.numeric(d$count)
head(d)
class(d$count)
str(d)

# Remove zeros and NAs

d = d[d$count>0,]
length(unique(d$count))
d = na.omit(d)
dim(d)
length(unique(d$count))

#===================================================================
# TIME data

# Explore range of years
unique(d$year)
class(d$year)

# Change year to numeric
d$year = as.numeric(d$year)
unique(d$year)
class(d$year)

#Explore month column
unique(d$month)
class(d$month)

# Change month to numeric
d$month = as.numeric(d$month)
head(d$month, 20)

# Convert months to decimal-months
d$dec_month = d$month/12
head(d)

# Remove old month
d = d[,-c(4)]

# Explore julian day column
class(d$julianday)
length(unique(d$julianday))

# Make day column numeric
d$julianday = as.numeric(d$julianday)
head(d)
str(d)

# Add year to month column to create decimal year
d$dec_year = d$year+d$dec_month
head(d)
length(unique(d$dec_year))

# Remove other unnecessary date columns
d = d[,-c(3,4,7)]
head(d)

#==================================================================
# SITE data

# Data for sites is listed by lat-longs
# USE 'ROUND_ANY' Wickham package to round lat-longs to nearest 2
library('plyr')
?round_any
d$lat = round_any(d$latitude, 1, f = round)
d$long = round_any(d$longitude, 1, f = round)
head(d)

# Paste together lat longs and look for uniques
d$lat_long = paste(d$lat, d$long, sep = '_')
head(d)
length(unique(d$lat_long))
unique(d$lat_long)      
 
  # There are 80 unique sites
  # Check for frequency of each site
site.df = data.frame(table(d$lat_long))
site.df[order(site.df$Freq),]

# Remove old lat long columns
d = d[,-c(1,2,6,7)]
head(d)
#=================================================================
# Arrange data by SITE, SPECIES, YEAR

# Create datasetID file
d$datasetID = rep(200, nrow(d))
head(d)
d1 = ddply(d,.(datasetID, lat_long, dec_year, species), summarize, count = max(count))
dim(d1)
head(d1, 100)
d1[3000:3100,]

# Change site to factor
d1$lat_long = factor(d1$lat_long)
head(d1)
summary(d1)

# Check for adequate species number at given site
test = ddply(d1, .(lat_long), summarize,richness = length(unique(species)))
head(test[order(test$richness),])

# All sites passed test with at least 10 species

# Test for adequate times (at least 5 time samples per site)
test = ddply(d1, .(lat_long), summarize, ntime = length(unique(dec_year)))
head(test[order(test$ntime),], 20)

# Several (17) sites had less than 5 time samples, so need to remove
bad_sites = c('29_-74','29_-80','29_-81','30_-80','31_-80','32_-79','34_-75','39_-70','30_-81','31_-81','32_-78','32_-80','32_-81','34_-79','33_-79','33_-80','44_-63')
length(bad_sites)

# reformat dataset without bad sites
d = d1[!d1$lat_long %in% bad_sites,]
head(d)
dim(d)
summary(d)
length(unique(d$lat_long))

# Finished