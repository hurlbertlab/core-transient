# Install and load necessary packages:

install.packages('reshape2')
library(reshape2)

# Get data:

setwd('/Users/bsevans/Desktop/core-transient-datasets/raw_datasets/dataset_228RAW')

hb = read.csv('hb_bird.txt')
mk = read.csv('mk_bird.txt')
rp = read.csv('rp_bird.txt')
sm = read.csv('sm_bird.txt')

reshape.fun = function(sampling.site, site.code){
  # Remove X's from the year column:
    names(sampling.site) = gsub('X', '',names(sampling.site))
  # Melt from wide to long format:
    dClean = melt(hb, id.vars = 'Bird.Species')
  # Set column names:
    names(dClean) = c('species','year','count')
  # Remove rows of summary data:
    dClean = dClean[dClean$species!='Total  (all Species)' &
                  dClean$species!= 'Number of Species',]
  # Replace "t" (trace?) values with 0's:
    dClean$count = factor(gsub('t', 0, dClean$count))
  # There is some r's in there connected with numbers, remove them:
    dClean$count = gsub('r', '', dClean$count)
  # Convert count and year data to numeric:
    dClean$count = as.numeric(dClean$count)
    dClean$year = as.numeric(levels(dClean$year))[as.integer(dClean$year)]
  # Add a site column (and arrange as the first column):
    site.name = paste('d228',site.code, sep ='_')
    dClean$site = factor(rep(site.name, length(dClean$year)))
    dClean = dClean[,c(4,1:3)]
  # Remove 0's:
    dClean = dClean[dClean$count>0,]
    dClean
   }

d228 = rbind(reshape.fun(hb,'hb'),reshape.fun(mk,'mk'),
      reshape.fun(rp,'rp'),reshape.fun(sm,'sm'))

setwd('/Users/bsevans/Desktop/core-transient-datasets/formatted_datasets')

write.csv(d228, 'dataset_228.csv', row.names = F)

