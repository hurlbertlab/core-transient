##########################################################
# Dataset 236 Chilean Small Mammals  Core-transient analysis
# Metadata for species occupancy: http://esapubs.org/archive/ecol/E094/084/metadata.php
# Metadata for dryad dataset: http://datadryad.org/resource/doi:10.5061/dryad.sb175.2
# Additional metadata for dryad: http://onlinelibrary.wiley.com/doi/10.1111/mec.12572/full
# Metadata for Ernest life history dataset: http://www.esapubs.org/archive/ecol/E084/093/default.htm#data
# Metadata for pantheria dataset: http://esapubs.org/Archive/ecol/E090/184/metadata.htm
##########################################################

# setwd('C:/Users/wlarsen/Desktop')

# Get propOcc dataset:

dataset = read.csv('propOcc_236wjl.csv')

# Only want control plots, remove experimental plots

goodsites = c(3,6,11,14)

dataset1 = dataset[dataset$site %in% goodsites,]

# layout(matrix(c(6,1,2,3,4,5), 3,2, byrow = TRUE))
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

### Dryad dataset
dryad = read.csv('Data4Dryad.csv')
dryadsp = dryad[dryad$TAXON %in% 'mammals',]
dryadsp$SPECIES = gsub("_"," ",dryadsp$SPECIES)
# Extract needed species
dryadsp = dryadsp[dryadsp$SPECIES %in% speclist,]
# Remove unneccessary columns
dryadsp = dryadsp[-1]
head(dryadsp)
# Does not have an entry for Octodon degus



### Ernest life history dataset
lifehist = read.csv('Mammal_lifehistories_v2.csv')
lifehist$scientificname = paste(lifehist$Genus,lifehist$species,sep=" ")
lifehist = lifehist[c(15,5:14)]

# This dataset has 'Abrocoma bennetti', it should be 'Abrocoma bennettii'
lifehist$scientificname[lifehist$scientificname == 'Abrocoma bennetti'] = 'Abrocoma bennettii'

# Extract needed species
lifehistsp = lifehist[lifehist$scientificname %in% speclist,]
head(lifehistsp)

# In this dataset, if a value is not available, a -999 is put in its place, replacing with NA's
lifehistsp[lifehistsp == -999] = NA_character_

# Remove unneccessary column:
lifehistsp = lifehistsp[-11]

head(lifehistsp)
# Does not have an entry for Abrothrix genus, Thylamys elegans, or Chelemys megalonyx


### Pantheria dataset
pantheria = read.csv('PanTHERIA_1-0_WR05_Aug2008.csv')

# Extract needed species
pantheria.sp = pantheria[pantheria$MSW05_Binomial %in% speclist,]

# In this dataset, if a value is not available, a -999 is put in its place, replacing with NA's
pantheria.sp[pantheria.sp == -999] = NA_character_

# Many columns unnecessary:
unusedfields = c('MSW05_Order','MSW05_Family','MSW05_Genus','MSW05_Species','References')

# Also removing columns with all NA's (no usable data):
for (i in 1:dim(pantheria.sp)[2]){
  
  if(sum(is.na(pantheria.sp[,i]))==9){
    
    unusedfields = c(unusedfields, names(pantheria.sp)[i])
    
  }
  
}
unusedfieldsind = which(names(pantheria.sp) %in% unusedfields)
pantheria.sp = pantheria.sp[-unusedfieldsind]

head(pantheria.sp)

### Merging the three datasets
# Merging dryad and life history datasets first
merge1= merge(dryadsp,lifehistsp, by.x = "SPECIES", by.y = "scientificname", all=TRUE)
# Check:
dim(dryadsp)
dim(lifehistsp)
dim(merge1)
# Merging first merge dataset with pantheria.sp
merge2 = merge(merge1,pantheria.sp, by.x = "SPECIES", by.y = "MSW05_Binomial", all=TRUE)
dim(merge1)
dim(pantheria.sp)
dim(merge2)
# Merge with avgdataset
datasetm = merge(avgdataset,merge2,by.x = "speclist",by.y="SPECIES",all=TRUE)
# Check:
dim(avgdataset)
dim(merge2)
dim(datasetm)


plot(datasetm$avgpropOcc,datasetm$ENV.HARSHNESS)
abline(lm(datasetm$ENV.HARSHNESS~datasetm$avgpropOcc))

plot(datasetm$avgpropOcc,datasetm$GEOGRAPHIC.COVERAGE)
abline(lm(datasetm$GEOGRAPHIC.COVERAGE~datasetm$avgpropOcc))

plot(datasetm$avgpropOcc,datasetm$RAINFALL.UNPREDICTABILITY)
abline(lm(datasetm$RAINFALL.UNPREDICTABILITY~datasetm$avgpropOcc))

plot(datasetm$avgpropOcc,datasetm$RESIDUAL.BODY.SIZE)
abline(lm(datasetm$RESIDUAL.BODY.SIZE~datasetm$avgpropOcc))

plot(datasetm$avgpropOcc,datasetm$X6.1_DietBreadth)
abline(lm(datasetm$X6.1_DietBreadth~datasetm$avgpropOcc))


