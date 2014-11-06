# This script creates histograms of each site with density lines, bimodality index,
# mean, and number of time samples.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

# Set read and write directories:

source('scripts/R-scripts/core-transient_analysis.R')

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

#----------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#==================================================================================*

ct.hist = function(prop.df, outSummary) {
  # Set breaks and band width for the histogram:
  bw = (max(prop.df$prop.yrs)-min(prop.df$prop.yrs))/10
  brks = seq(min(prop.df$prop.yrs), max(prop.df$prop.yrs),bw)
  # Plot labels:
  main = paste('Site ', outSummary[,2], paste('(', outSummary[,4],', ', outSummary[,5],')', sep = ''))
  sub = bquote(b == .(round(outSummary[,12], 2)) ~ '    '~
                 mu == .(round(outSummary[,13],2)) ~ '    '~
                 t == .(outSummary[,6]))
  # Plot data: 
  ggplot(prop.df, aes(x=prop.yrs)) +
    # Add histogram:
    geom_histogram(aes(y = ..density..), breaks = brks, right = F,
                   fill = 'gray', color = 1) +
    # Add density:
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
# ---- Function to return core-transient summary analysis ----
#==================================================================================*

return.ct.hist = function(dataset, site, threshold){
  # Extract proportional data frame:
  prop.list = prop.by.year(dataset, site)
  prop.df = prop.list[['prop.df']]
  yrs = prop.list[['years']]
  # Summary table output:
  outSummary = ctSummary(dataset, site, prop.df, yrs, threshold)
  # Graphical output: 
  siteHistogram = ct.hist(prop.df, outSummary)
  # Output
  return(siteHistogram)
}

#----------------------------------------------------------------------------------*
# ---- Generate output across sites  ----
#==================================================================================*

# Histogram for a given dataset, site, and threshold:

return.ct.hist(236, 'd236_ew', .33)

# Generate output lists across sites + datasets:

sites = unique(d$site)
out.plots = list()

for(i in 1:length(sites)){
  out.plots[[i]] = return.ct.hist(dID[i],sites[i],.33)
}

# Add site names to the plot outs

names(out.plots) = out.frame$site


# Write all of the plot outs:

plot.fun = function(i){
  out_name = paste('/plots/histogram_',out.frame$site[i],'.pdf', sep ='')
  out = paste(out_dir, out_name, sep = '')
  pdf(out, width = 6.5, height = 5.5)
  plot(out.plots[[i]])
  dev.off()
}

for (i in 1:length(out.plots)){
  plot.fun(i)
}


