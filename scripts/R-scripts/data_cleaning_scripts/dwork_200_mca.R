# Formatting Dataset 200

# Get Data

getwd()
setwd('C:/Users/auriemma/core-transient/')
d = read.csv('raw_datasets/dataset_200.csv')
dim(d)
head(d)
str(d)

#=========================================================
# SITE data

# Data for sites is listed by lat-longs....
#.........



#=========================================================
# SPECIES data

# Create species vector
sp = d$sname
length(unique(sp))

# Capitalize all to remove letter case error
d$species = toupper(d$sname)
head(d$species)

  # Compare with original
length(unique(sp))
length(unique(d$species))
  # No difference, no error in capitalization

# Search for species to remove
unique(d$species)
remove_sp = c('UNIDENTIFIED FISH','')

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

head(d)        
  
#===========================================================================
# COUNT data

length(unique(d$count))
summary(d)
