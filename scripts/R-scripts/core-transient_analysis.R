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
    colour="black", fill="gray") + 
    geom_density(alpha=.2, fill="blue") + 
    labs(title= 'Proportional densities at Eastern Wood',
         x = 'Proportion of years', y = 'Density of species/year') +
    theme(axis.text = element_text(size=14, color = 'black'),
          axis.title = element_text(size=18),
          title = element_text(size=22),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank())

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

#----------------------------------------------------------------------------------*
# ---- CALCULATE THE CORE-TRANSIENT RICHNESS COMPONENTS BY YEAR ----
#----------------------------------------------------------------------------------*

# Designate species as core or trans

head(df1)

df1$CT = ifelse(df1$prop.yrs>=core.thresh,'core',
                ifelse(df1$prop.yrs<=trans.thresh,'transient',NA))

# Merge the eastern wood frame with the designation of species

d226a = merge(d226, df1, by.x = 'species',by.y ='sp')

# Calculate the total, core, and transient richness by year

year = sort(unique(d226$year))
sp.rich = numeric()
core.rich = numeric()
trans.rich = numeric()
for(i in 1:length(year)){
  sp.rich[i] = length(unique(d226a[d226a$year == year[i],'species']))
  core.rich[i] = length(unique(d226a[d226a$year == year[i]
                        & d226a$CT == 'core','species']))
  trans.rich[i] = length(unique(d226a[d226a$year == year[i]
                          & d226a$CT == 'trans','species']))
  }

# Create a dataframe of the above and calculate the proportional
# richness for core and transient species for each year:

rich.yr.frame = data.frame(year, sp.rich,core.rich,trans.rich)
rich.yr.frame$prop.core = core.rich/sp.rich
rich.yr.frame$prop.trans = trans.rich/sp.rich

ggplot(rich.yr.frame, aes(x = year, y = prop.core)) + geom_point()
ggplot(rich.yr.frame, aes(x = year, y = prop.trans)) + geom_point()

#----------------------------------------------------------------------------------*
# ---- Generalizing to run on any dataset ----
#==================================================================================*

coreTrans = function(dataset, site, threshold){
  d = d[d$datasetID == dataset & d$site == site,] # Subsets data by dataset & site
  sp = unique(d$species)        # Generates a species list
  years = sort(unique(d$year))
  yrs = length(years)  # Generates a list of years
  # For loop to calculate the proportion of years a species has been observed:
    prop.yrs = numeric()
    for (i in 1:length(sp)){                        
      prop.yrs[i] = length(unique(d[d$species == sp[i],'year']))/yrs
    }
    d1 = data.frame(sp,prop.yrs)  # Dataframe of species and proportion of years
  # Subset into core and transient:
    core.thresh = 1 - threshold   # Threshold for core species
    trans.thresh = threshold      # Threshold for transient species
    sp.core = d1[d1$prop.yrs>=core.thresh,]
    sp.trans = d1[d1$prop.yrs<=trans.thresh,]
  # Assign species to core or transient status:
    d1$CT = ifelse(d1$prop.yrs>=core.thresh,'core',
                  ifelse(d1$prop.yrs<=trans.thresh,'transient',NA))
  # Merge with the original data frame:
    d = merge(d, d1, by.x = 'species', by.y = 'sp')[,-6]
  # Function to calculate richness indices and output it as a 1-row dataframe:
    rich.indis = function(dataset,site,year, threshold){
      if (year!='all') d = d[d$year == year,]
      rich.total = length(unique(d[,1]))  
      rich.core = length(unique(d[d$CT=='core',1]))
      rich.trans = length(unique(d[d$CT=='transient',1]))
      prop.core = rich.core/rich.total
      prop.trans = rich.trans/rich.total
      out = data.frame(dataset, site, year, threshold,
                       rich.total, rich.core, rich.trans, 
                      prop.core, prop.trans)
      names(out) = c('datasetID','site','year','threshold',
                     'total_richness','core_richness','trans_richness',
                     'prop_core','prop_trans')
      out
      }
  # Calculate richness indices across years for the study:
    r.across.years = rich.indis(dataset, site, 'all',threshold)
  # Calculate richness indices for each year of the study:
    r.by.yr = list()
    for(i in 1:length(years)){
      r.by.yr[[i]] = rich.indis(dataset, site, years[i], threshold)
      }
    r.by.yr = rbind.fill(r.by.yr)
  # Graphical output: 
    # Histogram across years
      site.histogram = ggplot(d1, aes(x=prop.yrs)) + 
        geom_histogram(aes(y=..density..), binwidth=.1,
                       colour="black", fill="gray") + 
        geom_density(alpha=.2, fill="blue") + 
        labs(title= 'Proportional density',
             x = 'Proportion of years', y = 'Density of species/year') +
        theme(axis.text = element_text(size=14, color = 'black'),
              axis.title = element_text(size=18),
              title = element_text(size=22),
              axis.line = element_line(colour = "black"),
              panel.background = element_blank())
  # Output
    list(r.across.years, r.by.yr, site.histogram)
}


out.test = coreTrans(226,'d226_ew',.33)

ggplot(out.test[[2]], aes(x = year, y = prop_core)) + geom_point()

ggplot(out.test[[2]], aes(x = year, y = prop_trans)) + geom_point()

# Run across all sites and datasets

sites = unique(d$site)
dID = numeric()

out1 = list()

for(i in 1:length(sites)){
    dID[i] = unique(d[d$site == sites[i],'datasetID'])
    out1[[i]] = coreTrans(dID[i],sites[i],.33)
  }


out1

