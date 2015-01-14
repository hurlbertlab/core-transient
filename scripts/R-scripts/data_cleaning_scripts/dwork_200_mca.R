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
summary(site)
length(site)
table(site)

#Remove samples with sample sizes less than 30
  # NEED to do this
site.df= data.frame(table(site))
site.df1 = data.frame(table(d$SampleID))
head(site.df1)

head(d)
head(site.df)
site1 = subset(site,?????
               
# Species check

species = d$Species
length(species)              
unique(species)

# Remove unspecified species (Low taxonomic resolution, lowest accepted is Family)
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
