#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Libraries:

library(plyr)
library(reshape)

# Set read and write directories:

in_dir = 'formatted_datasets/dornelas_unformat'
out_dir = 'formatted_datasets/dornelas_cleaned'


# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)

#----------------------------------------------------------------------------------*
# Dataset 46
#----------------------------------------------------------------------------------*

d = read.csv("formatted_datasets/dataset_46.csv")
d$site = substr(d$site,1,3)

write.csv(d, "formatted_datasets/dataset_46.csv", row.names = F)

#----------------------------------------------------------------------------------*
# Dataset 47
#----------------------------------------------------------------------------------*

d = read.csv("formatted_datasets/dataset_47.csv")
d$site = substr(d$site,1,nchar(as.character(d$site))-6)

write.csv(d, "formatted_datasets/dataset_47.csv", row.names = F)

#----------------------------------------------------------------------------------*
# Dataset 76
#----------------------------------------------------------------------------------*

d = read.csv("formatted_datasets/dataset_76.csv")

site1 = read.table(text = as.character(d$site), sep = "_", colClasses = "character")

lat = round_any(as.numeric(site1$V5),1)
lon = round_any(as.numeric(site1$V6),1)

d$site = paste(site1$V1, lat, lon, sep = '_')

write.csv(d, "formatted_datasets/dataset_76.csv", row.names = F)

#----------------------------------------------------------------------------------*
# Dataset 108
#----------------------------------------------------------------------------------*

t = read.csv(datasets[1])

t1 = t$site

t2 = transform(t1, site = colsplit(t1, split = "\\_", 
            names = c('dataset', 'location1','location2','location3','x','y')))

# Round site x and y locations to 2 degree blocks and paste:

t2$site.x = round_any(t2$site.x, 2)

t2$site.y = round_any(t2$site.y, 2)

t2$site = paste(t2$site.dataset, t2$site.x, t2$site.y, sep = '_')

# Change site column in the original dataset:

t$site = t2$site

# Sum counts to the new sites:

t2 = ddply(t,.(datasetID, site,species,year), summarise, count = sum(count))

# Write file:

write.csv(t2, 'formatted_datasets/dataset_108.csv', row.names = F)

#----------------------------------------------------------------------------------*
# Dataset 110
#----------------------------------------------------------------------------------*
# Note: This one may be a problem. Lat's and Lon's may be required here.

t = read.csv(datasets[2])

t1 = t$site

t2 = transform(t1, site = colsplit(t1, split = "\\(", names = c('dataset')))

t3 = t2$site.dataset

t4 = transform(t3, site = colsplit(t1, split = "\\_", names = c('dataset')))

t5 = paste(t4[,2],t4[,5], sep = '_')

# Change site column in the original dataset:

t$site = t5

# Sum counts to the new sites:

t2 = ddply(t,.(site,species,year), summarise, count = sum(count))

# Write file:

write.csv(t2, paste(out_dir, 'dataset_110.csv', sep = '/'), row.names = F)

#----------------------------------------------------------------------------------*
# Dataset 112
#----------------------------------------------------------------------------------*

t = read.csv(datasets[3])




