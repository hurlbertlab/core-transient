#This script calculates occupancy distributions and core and occasional species richness
#for BBS route with continuous data from 1996 - 2010.  It averages across values calculated for all possible 
#windows of size t (from 1:15) within the date range

###################################################################################*
# ---- SET-UP ----
###################################################################################*

#----------------------------------------------------------------------------------*
# ---- Libraries ----
#==================================================================================*

library(plyr)
library(ggplot2)

#----------------------------------------------------------------------------------*
# ---- Get data ----
#==================================================================================*

# Set read and write directories:

in_dir = 'formatted_datasets'

# Gather all files in directory:

datasets = list.files(in_dir, pattern="*.csv", full.names=T)
data.list = lapply(datasets, read.csv)

# The following function and line of code will change field names from density
# to count to ensure equivalent names (for rbinding):

name.changer = function(x){
  if (names(x)[5] == 'density') names(x)[5] = 'count'
  x
}

data.list = lapply(data.list, name.changer)

# Bind the list into a single dataframe that includes all datasets:

d = rbind.fill(data.list)

#----------------------------------------------------------------------------------*
# ---- Example run, Eastern Wood ----
#==================================================================================*

d226 = d[d$datasetID == 226,]

# Generate a species list:

sp = unique(d226$species)

# Calculate the number of years sampled:

yrs = length(unique(d226$year))

# Calculate the proportion of years each species was sampled:

prop.yrs = numeric()

for (i in 1:length(sp)){
  prop.yrs[i] = length(unique(d226[d226$species == sp[i],'year']))/yrs
}

df1 = data.frame(sp,prop.yrs)

ggplot(df1, aes(x=prop.yrs)) + 
  geom_histogram(aes(y=..density..), binwidth=.1,
    colour="black", fill="gray") + theme_bw() + 
    geom_density(alpha=.2, fill="blue")

# Threshold:

core.thresh = 2/3
trans.thresh = 1/3

# Subset into core and transient:

sp.core = df1[df1$prop.yrs>=core.thresh,]

sp.trans = df1[df1$prop.yrs<=trans.thresh,]

# Richness:

rich.total = length(df1[,1])

rich.core = length(sp.core[,1])

rich.trans = length(sp.trans[,1])

prop.core = rich.core/rich.total

prop.trans = rich.trans/rich.total

# LOOKING AT THE CHANGE IN CORE-TRANSIENT RICHNESS OVER TIME:

# Designate species as core or trans

head(df1)

df1$CT = ifelse(df1$prop.yrs>=core.thresh,'core',
                ifelse(df1$prop.yrs<=trans.thresh,'transient',NA))

# 
