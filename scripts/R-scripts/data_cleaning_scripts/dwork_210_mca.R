# Script for cleaning raw dataset number 210

#Get data
getwd()
list.files('raw_datasets')
setwd('C:/Users/auriemma/core-transient/raw_datasets')
list.files()
d = read.csv('dataset_210.csv')


# Explore data
head(d)
dim(d)
str(d)

# Removing biomass counts of 0

summary(d$biomass)

d = d[d$biomass> 0,]
summary(d$biomass)
dim(d)

# Site data 

summary(d$exp)
site = paste(d$field, d$plot, sep = '')
length(unique(site))
table(site)
    #All sites have significant sample sizes, so none removed

# Add site column to dataset d

d$site = paste(d$field, d$plot, sep = '')
head(d)
tail(d)

# Remove unnecessary columns

d1 = d[,c(3,10,11,12)]
head(d1)
d = d1
head(d)

# Raw species data

species = d$species
unique(species)

# Remove unwanted species names in dataset
remove_spp = c('Miscellaneous herbs','Miscellaneous grasses','Fungi','Miscellaneous litter','Mosses & lichens','Miscellaneous rushes','Miscellaneous legumes','Miscellaneous sedges','Miscellaneous forb','Miscellaneous sp.','Miscellaneous grasses 2','Miscellaneous woody plants','Pine needles','Sedges','Mosses & lichens 2','Miscellaneous herb')
species = species[!species%in%remove_spp]
unique(species)

# Dataset species column species removal
d1 = d[!d$species%in%remove_spp,]
dim(d1)
unique(d1$species)
d = d1
head(d)

# Time column
str(d)
unique(d$year)
year2 = strptime(d$year,'%Y')
str(year2)
year3 = as.numeric(format(year2,'%Y'))
str(year3)

# Numeric year into the dataset
d$year = year3

head(d)
tail(d)
str(d)

# Count column creation
library('plyr')
d1 = ddply(d,.(site,year,species), summarize, count = max(biomass))
head(d1)
summary(d1)
dim(d1)

# Add datasetID column
d1$datasetID = rep(210,length(d1[,1]))
head(d1)
d210 = d1[,c(5,1,3,2,4)]
head(d210)
str(d210)
