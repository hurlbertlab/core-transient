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
  
  # Create separate count vector
count = d$adults
head(count)
str(count)
  
  # Make all values of 0 and blanks 'NA'
count[count == 0] = NA
head(count)
summary(count)
count[count == ""] = NA
summary(count)

  # Add column to dataset and check if all data lines up correctly
d1$count = count
tail(d1,100)
d1[500:700,]
d1[1300:1600,]
d1[12334:12563,]

  # Remove NA's and 'adults' column
str(d1)
d1 = na.omit(d1)
head(d1)
d = d1[,-c(6)]
head(d,100)

  # Make count column numeric values
d$count1 = as.character(d$count)
head(d)
str(d)
d$count = as.numeric(d$count1)
str(d)
d = d[,-c(7,8)]
head(d)


