source('scripts/R-scripts/core-transient_analysis.R')

# Summary data by dataset:

out.by.datasets = ddply(out.frame, .(datasetID), summarize, 
                        mean.prop.core = mean(prop_core), 
                        se.prop.core = sd(prop_core)/sqrt(length(prop_core)),
                        mean.prop.core = mean(prop_trans), 
                        se.prop.core = sd(prop_trans)/sqrt(length(prop_trans)))
