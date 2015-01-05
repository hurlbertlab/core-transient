# Plot outputs:

library(ggplot2)
library(wesanderson)

# Get files:

occProp = read.csv('output/occProp.csv')
nTime = read.csv('output/nTime.csv')
outSummary = read.csv('data_source_table.csv')
ctSummary = read.csv('output/tabular_data/core-transient_summary.csv')
modeSummary = read.csv('output/tabular_data/ct_mode_summary.csv')
head(modeSummary)

ct.prop = ctSummary[,c(3:4,9:10)]
ct.prop$prop.nct = 1 - ct$prop.core - ct$prop.trans

ct.rich = ctSummary[,c(3:4,7:8)]
ct.rich$rich.nct = ct$rich.total - ct$rich.core - ct$rich.trans


