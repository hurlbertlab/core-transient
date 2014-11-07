# This script creates histograms of each site with density lines, bimodality index,
# mean, and number of time samples.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Get files:

prop.df = read.csv('output/prop.df.csv')
nTime = read.csv('output/Ntime.df.csv')
outSummary = read.csv('data_source_table.csv')

# Source core-transient functions:

source('scripts/R-scripts/tokeshi_function.R')

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

#----------------------------------------------------------------------------------*
# ---- Dashboard: Histograms ----
#==================================================================================*

# Histogram for a given site, and threshold:

ct.hist('d226_ew', .33)

# Generate output lists across sites + datasets:

sites = unique(prop.df$site)

# Plot across sites, output as one pdf: 

out.plots = list()
for(i in 1:length(sites)){
  out.plots[[i]] = ct.hist(sites[i],.33)
}

pdf('output/plots/CT_histograms.pdf', 
    width = 6.5, height = 5.5, onefile = T)
  out.plots
dev.off()

#----------------------------------------------------------------------------------*
# ---- Get and run data ----
#==================================================================================*

# For loop to return Tokeshi P's for each site:

sites = unique(prop.df$site)

out.list = list()

for(i in sites){
  out.list[[i]] = tokeshiFun(i, 1/3)
}

tokeshi.outs = na.omit(rbind.fill(out.list))

tokeshi.outs$bimodality = ifelse(tokeshi.outs$Pl<=0.05&tokeshi.outs$Pr<=0.05,
    'strongly bimodal', ifelse(tokeshi.outs$Pl<0.25&tokeshi.outs$Pr<0.25, 
    'bimodal', ifelse(tokeshi.outs$Pl<0.5&tokeshi.outs$Pr<0.5, 
    'weakly bimodal', ifelse(tokeshi.outs$Pl<=0.05&tokeshi.outs$Pr<=0.5,
    'weakly bimodal', ifelse(tokeshi.outs$Pl<=0.5&tokeshi.outs$Pr<=0.05,
    'weakly bimodal', 'not bimodal')))))

# Tabular output:
 
tokeshi.outs

# Plot output:

ggplot(tokeshi.outs, aes(x = Pr, y = Pl,col = bimodality)) +
  geom_point() +
  xlab('P(F > f), core species')+
  ylab('P(F > f), transient species')+
  geom_segment(aes(x = .05, y = 0, xend = .05, yend = 1), 
               color = 1, size = .5, linetype  = 1) +
  geom_segment(aes(x = 0, y = .05, xend = 1, yend = .05), 
               color = 1, size = .5, linetype = 1) +
  geom_segment(aes(x = .25, y = 0, xend = .25, yend = 1), 
               color = 1, size = .5, linetype  = 2) +
  geom_segment(aes(x = 0, y = .25, xend = 1, yend = .25), 
               color = 1, size = .5, linetype =2) +
  geom_segment(aes(x = .5, y = 0, xend = .5, yend = 1), 
               color = 1, size = .5, linetype  = 3) +
  geom_segment(aes(x = 0, y = .5, xend = 1, yend = .5), 
               color = 1, size = .5, linetype = 3) +
  ggtitle('Tokeshi bimodality test across sites')+
  # Add themes:
   theme(axis.text = element_text(size=14, color = 1),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2),
        title = element_text(size=18, vjust = 2, face = 'bold'),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"))


