# Libraries:

library(reshape2)
library(plyr)
library(ggplot2)
library(wesanderson)

# Get files:

occProp = read.csv('output/occProp.csv')
nTime = read.csv('output/nTime.csv')
outSummary = read.csv('data_source_table.csv')
ctSummary = read.csv('output/tabular_data/core-transient_summary.csv')
modeSummary = read.csv('output/tabular_data/ct_mode_summary.csv')
head(modeSummary)

#----------------------------------------------------------------------------------*
# ---- Stacked barplot ----
#==================================================================================*

#----------------------------------------------------------------------------------*
# Using proportions at a given site
#----------------------------------------------------------------------------------*

# Make prop data frame with an "other" field representing neither core nor 
# transient species:

ct.prop = ctSummary[,c(3:4,9:10)]
ct.prop$other = 1 - ct$prop.core - ct$prop.trans

# Convert from a wide to long formatted data frame:

ct.prop = melt(ct.prop, id.vars=c('system','taxa'))

# Calculate the average proportions across sites for system and taxa:

ctPropSystem = ddply(ct.prop,.(system, variable),summarise,mean(value))

  names(ctPropSystem) = c('system','class','prop')

ctPropTaxa = ddply(ct.prop,.(taxa, variable),summarise,mean(value))

  names(ctPropTaxa) = c('taxa','class','prop')

# Plot data:

ggplot(data=ct.prop, aes(x=taxa, y=value, fill=class)) + geom_bar(stat="identity")





#----------------------------------------------------------------------------------*
# Using richness across sites
#----------------------------------------------------------------------------------*

ct.rich = ctSummary[,c(3:4,7:8)]
ct.rich$rich.nct = ct$rich.total - ct$rich.core - ct$rich.trans






