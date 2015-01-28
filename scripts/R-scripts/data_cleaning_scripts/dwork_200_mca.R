# Formatting Dataset 200

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
# Did not remove any of these listed, but made note of each

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
# USE 'ROUND_ANY' Wickham package, then paste together and look for uniques
      # Ideal number is about 50 or more sites
      # If too many, then reduce to 2-degree lat-long blocks and see if ~50 sites


