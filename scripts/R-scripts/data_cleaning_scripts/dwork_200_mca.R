# Formatting Dataset 200

# Get Data

getwd()
setwd('C:/Users/auriemma/core-transient/')
d = read.csv('raw_datasets/dataset_200.csv')
dim(d)
head(d)
tail(d)
str(d)


# Site data
site = d$SampleID
unique(site)
summary(site)
length(site)
table(site)
length(unique(site))

# Listing sites by number of rows present in dataset

library(plyr)
ex = ddply(d, .(SampleID), 'nrow')
head(ex)
ex2 = ex[order(ex$nrow),]
head(ex2)
head(ex2, 40)
summary(ex2)


# Species check

species = d$Species
length(species)              
unique(species)

# Remove unspecified species (Ones of low taxonomic resolution, lowest accepted is Family)
remove_spp = c('Copepoda','Gastropoda','Decapoda','Amphipoda','Ostracoda','Cnidaria','Anthozoa','Echinodermata','Nematoda','Hydrozoa','Nudibranchia','Bryozoa','Mollusca','Crustacea','Cephalopoda','Scyphozoa','Pisces','Porifera','Annelida','Chaetognatha','Bivalvia','Octopoda','Isopoda','Ctenophora','Insecta','Scaphopoda','Zoanthidea','Ophiurida')
length(remove_spp)
species = species[!species%in%remove_spp]
length(species)
unique(species)

# Add fixed species column
d1 = d[!d$Species%in%remove_spp,]
dim(d1)
length(species)
head(d1)

#Remove unwanted columns

d = d1[,-c(1)]
head(d)
dim(d)
length(d$ID)

# Year

year = as.numeric(d$Year)
d$year = year
head(d)

# Remove non-numeric year column

d = d[,-c(2)]
head(d)
