#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Libraries:

library(plyr)
library(reshape)

# Set read and write directories:

in_dir = 'dornelas_unformat'
out_dir = 'dornelas_cleaned'


# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)

#----------------------------------------------------------------------------------*
# Dataset 108
#----------------------------------------------------------------------------------*

t = read.csv(datasets[1])

t1 = t$site

df <- data.frame(ID=11:13, FOO=c('a|b','b|c','x|y'))
t2 = transform(t1, site = colsplit(t1, split = "\\_", names = c('dataset', 'location1','location2','location3','x','y')))

# Round site x and y locations to 2 degree blocks and paste:

t2$site.x = round_any(t2$site.x, 2)

t2$site.y = round_any(t2$site.y, 2)

t2$site = paste(t2$site.dataset, t2$site.x, t2$site.y, sep = '_')

# Change site column in the original dataset:

t$site = t2$site

# Sum counts to the new sites:

t2 = ddply(t,.(site,species,year), summarise, count = sum(count))



