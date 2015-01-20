# Formatting dataset 208: Landis long-term insect database

# Get data

getwd()
setwd('c:/Users/auriemma/core-transient/raw_datasets')
d = read.csv('dataset_208.csv')

# Exploring dataset

head(d)
names(d)
dim(d)
str(d)

# Remove columns not needed

d1 = d[,-c(1,7,8,10,11)]
head(d1)
head(d1, 50)
head(d1, 100)
head(d1,10)

d = d1
summary(d)

# SPECIES column

d$species = toupper(d$species)
sp = d$species
unique(sp)
length(sp)
  
  # Removing unwanted species
remove_sp = c('SOMETHING ELSE',"")
d1 = d[!d$species %in% remove_sp,]
unique(d1$species)
head(d1)
d = d1

# COUNT column
head(d)
summary(d)
str(d$adults)

  # Change adults row to numeric
d1 = d
head(d1)

# Removing rows with count of 0
d1 = d[d$adults>0,]
