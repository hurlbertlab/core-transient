# This script creates histograms of each site with density lines, bimodality index,
# mean, and number of time samples.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Get files:

prop.df = read.csv('output/prop.df.csv')
nTime = read.csv('output/Ntime.df.csv')
outSummary = read.csv('data_source_table.csv')

# Source Tokeshi function for output:

source('scripts/R-scripts/tokeshi_function.R')

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

#----------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#==================================================================================*

ct.hist = function(site,h) {
  # Get data, subset to a given site:
    prop.df = prop.df[prop.df$site == site,]
    outSummary = outSummary[outSummary$dataset_ID == unique(prop.df$dataset),]
    nTime = nTime[nTime$site == site,]
  # Get summary stats for subtitle:
    system = as.character(outSummary$system)
    taxa = as.character(outSummary$taxa)
    bimod.parm = round(bimodality(prop.df$occ, nTime$nt),2)
    mu = round(mean(prop.df$occ))
    tokeshi = tokeshiFun(site, h)
  # Plot labels:
    main = paste('Site ', site, paste('(', system,', ', taxa,')', sep = ''))
    sub = bquote(b ~ '=' ~ .(bimod.parm) ~ '    '~
            mu ~ '=' ~ .(mu) ~ '    '~
            t ~ '=' ~ .(nTime$nt) ~ '    '~
            P['core'] ~ '=' ~ .(round(tokeshi$Pr,3)) ~ '    '~
            P['trans'] ~ '=' ~ .(round(tokeshi$Pl,3)))
  # Set breaks and band width for the histogram:
    bw = (max(prop.df$occ)-min(prop.df$occ))/10
    brks = seq(min(prop.df$occ), max(prop.df$occ),bw)
  # Plot data: 
    ggplot(prop.df, aes(x=occ)) +
      geom_histogram(aes(y = ..density..), breaks = brks, right = F,
                   fill = 'gray', color = 1) +
      geom_density(alpha=.2, fill="blue") + 
      # Add labels:
        xlab('Proportion of temporal samples') + ylab('Density') + 
        ggtitle(bquote(atop(.(main), atop(.(sub))))) +
      # Add themes:
        theme(axis.text = element_text(size=14, color = 1),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 2),
          title = element_text(size=18, vjust = -1),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(.5,.5,1.5,1), "lines"))
}

#----------------------------------------------------------------------------------*
# ---- Generate plot output across sites  ----
#==================================================================================*

# Histogram for a given dataset, site, and threshold:

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

