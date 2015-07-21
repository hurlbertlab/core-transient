##########################################################
# Dataset 236 Chilean Small Mammals  Core-transient analysis
# Metadata can be found at http://esapubs.org/archive/ecol/E094/084/metadata.php
##########################################################

# setwd('C:/git/core-transient')

# Get propOcc dataset:

dataset = read.csv('data/propOcc_datasets/propOcc_236.csv')

# Only want control plots, remove experimental plots

goodsites = c(3,6,11,14)

dataset1 = dataset[dataset$site %in% goodsites,]

layout(matrix(c(6,1,2,3,4,5), 3,2, byrow = TRUE))
hist(dataset$propOcc)
hist(dataset$propOcc[dataset1$site == 3])
hist(dataset$propOcc[dataset1$site == 6])
hist(dataset$propOcc[dataset1$site == 11])
hist(dataset$propOcc[dataset1$site == 14])

tranlist = data.frame(table(dataset1$species[dataset1$propOcc<0.5]))$Var1[as.integer(data.frame(table(dataset1$species[dataset1$propOcc<0.5]))$Freq)>0]

corelist = data.frame(table(dataset1$species[dataset1$propOcc>0.5]))$Var1[as.integer(data.frame(table(dataset1$species[dataset1$propOcc>0.5]))$Freq)>0]

dataset1$species = factor(dataset1$species)

# Found meaning of species codes from metadata, switching to full species names

speccodes = levels(dataset1$species)

speclist = c('Abrocoma bennettii','Abrothrix longipilis','Abrothrix olivaceus','Chelemys megalonyx','Octodon lunatus','Thylamys elegans','Octodon degus','Oligoryzomys longicaudatus','Phyllotis darwini')

# Make a column with the average propOcc for each species
avgpropOcc = c()

for (i in 1:length(speclist)){
  
  avgpropOcc[i] = sum(dataset1[dataset1$species == speccodes[i],]$propOcc)/length((dataset1[dataset1$species == speccodes[i],]$propOcc))
  
}

# Make dataset with average propOcc and species, remove NAs
avgdataset = data.frame(speclist,speccodes, avgpropOcc )
avgdataset = avgdataset[order(avgpropOcc),]
avgdataset = na.omit(avgdataset)
avgdataset$speclist = factor(avgdataset$speclist)

# Set species list and species codes to be in the order I'm using in the dataset

speccodes = avgdataset$speccodes

speclist = avgdataset$speclist

# Create core column, logical TRUE if core, FALSE if transient
avgdataset$core = rep(FALSE,length(avgdataset$speclist))

for (i in 1:length(avgdataset$speclist)){
  
  if (avgdataset$avgpropOcc[i] >0.5) {
    avgdataset$core[i] = TRUE
  }
   
  
}


# Plot
barplot(avgdataset$avgpropOcc, names.arg = avgdataset$speccodes)

# setwd('C:/Users/wlarsen/Downloads')

# Dryad dataset
dryad = read.csv('Data4Dryad.csv')
dryadm = dryad[dryad$TAXON %in% 'mammals',]
dryadmsp = dryadm[dryadm$SPECIES %in% speclist,]
dryadmsp = dryadmsp[c(4,5,1,6,2,8,3,7),]
# Does not have an entry for Octodon degus

# Ernest life history dataset
lifehist = read.csv('Mammal_lifehistories_v2.csv')
# Does not have an entry for Abrothrix genus

pantheria = read.csv('PanTHERIA_1-0_WR05_Aug2008.csv')
pantheria.sp = pantheria[pantheria$MSW05_Binomial %in% speclist,]
pantheria.sp = pantheria.sp[-c(1:4)]
# Reorder according to speclist
pantheria.sporder = pantheria.sp[c(1,4,6,2,7,9,8,3,5),]


