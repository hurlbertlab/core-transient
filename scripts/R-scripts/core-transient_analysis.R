# This script creates a table of core-transient summary statistics by site.

#----------------------------------------------------------------------------------*
# ---- Set-up ----
#==================================================================================*

source('scripts/R-scripts/ct_proportion_frame.R')

#----------------------------------------------------------------------------------*
# ---- Function for calculating bimodality ----
#==================================================================================*
# Note: bimodality is the fraction of species occurring at either end of occupancy
# distribution

bimodality = function(occs, n.time) {
  maxvar = var(c(rep(1/n.time,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
  }

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
# Note: From meeting with Allen, 10/30/14, not currently tested within this script.

fitbeta = function(dataID, not1) {
  occs = out.list[[1]][[dataID]]$prop.yrs
  occs[occs == 1] = not1
  shape.params = fitdistr(occs, "beta", list(shape1 = 2, shape2 = 2))
  return(as.vector(shape.params$estimate))
  }

#----------------------------------------------------------------------------------*
# ---- Tokeshi functions ----
#==================================================================================*

# Tokeshi's function to determine if there is a greater number of individuals in a
# bin than random chance (F>f):

tokeshi.rl.fun = function(N, right.or.left, h){
  outs = NULL
  ins = right.or.left:N
  for(i in ins){
    o = (factorial(N)/(factorial(i)*factorial(N-i)))*h^i*(1-h)^(N-i)
    outs = c(outs, o)
  }
  return(sum(outs))
}

# Tokeshi's function to determine if there are left or right modes that are
# greater than expected under a null distribution:

tokeshi.c.fun = function(N, nr, nl, h){
  outs = NULL
  for (i in nl:(N - nr)){
    for(j in nr:(N - i)){
      o = (factorial(N)*h^(i + j)*(1-2*h)^(N-i-j))/
        (factorial(i)*factorial(j)*factorial(N-i-j))
      outs = c(outs, o)
    }
  } 
  sum(outs)
}

# Function to run Tokeshi's bimodality test for a given site:

tokeshiFun = function(site, h){
  df = prop.df[prop.df$site == site, ]  # Subset to a given site
  N = length(df[,1])              # The total number of species at the site
  h = h                           # The frequency interval 
  nr = length(df[df[,4]>=1-h,1])  # The number of species in the upper class
  nl = length(df[df[,4]<=h,1])    # The number of species in the lower class
  Pc = tokeshi.c.fun(N, nr, nl, h)    # Probability of left-or-right skew
  Pr = tokeshi.rl.fun(N, nr, h)       # Right mode probability
  Pl = tokeshi.rl.fun(N, nl, h)       # Left mode probability
  out.df = data.frame(site, N, h, nr, nl, Pc, Pr, Pl)
  return(out.df)
}

#----------------------------------------------------------------------------------*
# ---- Function to generate output summary dataset ----
#==================================================================================*
# Note the output of this function is a one line data frame.

ctSummary = function(dataset, site, prop.df, yrs, threshold){
  # Calculate bimodality of the dataset and site:
    bimodal = bimodality(prop.df$prop.yrs, yrs)
  # Subset into core and transient:
    core.thresh = 1 - threshold   # Threshold for core species
    trans.thresh = threshold      # Threshold for transient species
    sp.core = prop.df[prop.df$prop.yrs>=core.thresh,]
    sp.trans = prop.df[prop.df$prop.yrs<=trans.thresh,]
  # Assign species to core or transient status:
    prop.df$CT = ifelse(prop.df$prop.yrs>=core.thresh,'core',
                 ifelse(prop.df$prop.yrs<=trans.thresh,'transient',NA))
  # Merge with the original data frame:
    d = merge(d, prop.df, by.x = 'species', by.y = 'sp')[,-6]
  # Calculate richness indices:
    rich.total = length(unique(d[,1]))  
    rich.core = length(unique(d[d$CT=='core',1]))
    rich.trans = length(unique(d[d$CT=='transient',1]))
    prop.core = rich.core/rich.total
    prop.trans = rich.trans/rich.total
    summary.out = summary.table[summary.table[,1] == dataset,c(9,11)]
  # Output
    out = data.frame(dataset, site, threshold,summary.out[,1], summary.out[,2], yrs,
              rich.total, rich.core, rich.trans, 
              prop.core, prop.trans, bimodal, mean(prop.df$prop.yrs))
    names(out) = c('datasetID','site','threshold','system','taxa', 'yrs',
                    'total_richness','core_richness','trans_richness',
                    'prop_core','prop_trans', 'bimodality','mean')
    return(out)
    }

#----------------------------------------------------------------------------------*
# ---- Function to return core-transient summary analysis ----
#==================================================================================*

coreTrans = function(dataset, site, threshold){
  # Extract proportional data frame:
    prop.list = prop.by.year(dataset, site)
    prop.df = prop.list[['prop.df']]
    yrs = prop.list[['years']]
  # Summary table output:
    outSummary = ctSummary(dataset, site, prop.df, yrs, threshold)
  # Output
    list(prop.df,outSummary)
  }

#----------------------------------------------------------------------------------*
# ---- Generate output across sites  ----
#==================================================================================*

# Generate output lists across sites + datasets:

sites = unique(d$site)
dID = numeric()
out.raw = list()  # dataframe of 
out.frame = list()

for(i in 1:length(sites)){
  dID[i] = unique(d[d$site == sites[i],'datasetID'])
  out.raw[[i]] = coreTrans(dID[i],sites[i],.33)[[1]]
  out.frame[[i]] = coreTrans(dID[i],sites[i],.33)[[2]]
}

# Make the output summary frame into a single dataframe:

out.frame = rbind.fill(out.frame)

# Bind outputs into a single list:

out.list = list(out.raw, out.frame)
  names(out.list) = c('raw_output', 'summary_output')
  

# Write the tabular summary data output:

write.csv(out.frame, paste(out_dir,'/tabular_data/out_frame.csv', sep = ''), row.names = F)




