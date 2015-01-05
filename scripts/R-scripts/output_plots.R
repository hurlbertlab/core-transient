# Libraries:

library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)
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

ctProp = ctSummary[,c(3:4,9:10)]
ctProp$other = 1 - ct$prop.core - ct$prop.trans

# Convert from a wide to long formatted data frame:

ctProp = melt(ctProp, id.vars=c('system','taxa'))

# Calculate the average proportions across sites for system and taxa:

ctPropSystem = ddply(ctProp,.(system, variable),summarise,mean(value))

  names(ctPropSystem) = c('system','class','prop')

ctPropTaxa = ddply(ctProp,.(taxa, variable),summarise,mean(value))

  names(ctPropTaxa) = c('taxa','class','prop')

# Plot proportions by system:

ggplot(data=ctPropSystem, aes(x=system, y=prop, fill=class)) +
  geom_bar(stat="identity")

# Plot proportions by taxa:

ggplot(data=ctPropTaxa, aes(x=taxa, y=prop, fill=class)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = palette(wes.palette(5,'FantasticFox')))+
  xlab('Taxonomic group')+
  ylab('Proportion of sample')+
  ggtitle(bquote(bold('Proportional distribution of core and transient\nspecies by taxanomic group')))+
  theme(axis.text.x = element_text(size=14, color = 1, 
                                   angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=12, color = 1, hjust = 1),
        axis.title.x = element_text(size = 18, vjust = -1),
        axis.title.y = element_text(size = 18, vjust = 1.5),
        title = element_text(size=18, vjust = 2.5),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,.5,1.5,.5), "lines"))

 







#----------------------------------------------------------------------------------*
# Using richness across sites
#----------------------------------------------------------------------------------*

ct.rich = ctSummary[,c(3:4,7:8)]
ct.rich$rich.nct = ct$rich.total - ct$rich.core - ct$rich.trans






