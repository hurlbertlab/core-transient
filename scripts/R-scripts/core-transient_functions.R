# This script contains the functions used in the analyses that summarize
# core-transient data by site.

#==================================================================================*
# ---- BIMODALILITY ----
#==================================================================================*
# Functions:
# - bimodality: Calculates the bimodality metric developed by Allen and Ethan. 
#     Inputs: The occurances (prop by time) and # of time samples. 
#     Outputs: A single numeric bimodality value
#
# - fitbeta: Calculates the shape parameters for a fitted beta distribution for a
#   given site. 
#     Inputs: *** Inputs must be changed to run under the new data structure! 
#     Outputs: A vector of shape parameters.
#
# - tokeshi.rl.fun: Calculates the probability that a given mode (core or 
#   transient) is signficantly different than a null distribution. 
#     Inputs: N (# of species), the # of species in core or transient bins, 
#       and the frequency interval. 
#     Outputs: A single numeric p-value (P(F<f)).
#
# - tokeshi.c.fun: Calculates the probability that the modes of the core and 
#   transient bins are significantly different than a null distribution. 
#     Inputs: N (# of species), the # of species in the core and transient bins, 
#       and the frequency interval.
#     Outputs: A single numeric p-value (P(F<f))
# 
# - tokeshiFun: A wrapper function that calculates the tokeshi p-values for the core
#   and transient modes separately and combined.
#     Inputs: A site and the frequency interval
#     Outputs: A dataframe that includes the site, # of species, frequency interval,
#       # of core and transient species, P(core+trans), P(core), P(trans)
# 
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
  d = read.csv('output/prop.df.csv')
  df = d[d$site == site, ]  # Subset to a given site
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

#==================================================================================*
# ---- DATASET SUMMARY FUNCTIONS ----
#==================================================================================*
# Functions:
# - ctSummary: Produces summary data for one site. 
#     Inputs: Site and the threshold value for core and transient designation are
#       directly input. d (proportional occurence df), dst (data source table),
#       and nt (temporal samples df) are defined in the wrapper function. This is
#       to reduce the time it takes for the function to run.
#     Outputs: A one-row dataframe with dataset ID, site ID, threshold used,
#       the system, taxa, # of time samples, total, core, and transient richness
#       proportion of core and transient species,  the average proportion of 
#       occurance across species, bimodality (Allen + Ethan formula), and Tokeshi's
#       p-values for core-trans, core, and transient modes.
#
# - coreTrans: Wrapper function for the above:
#     Inputs: threshold for core- and trans- designation.
#     Outputs: Dataframe that includes all sites (rows) with fields as above.

#----------------------------------------------------------------------------------*
# ---- Function to generate output summary dataset ----
#==================================================================================*
# Note the output of this function is a one line data frame.

ctSummary = function(d, dst, nt, site, threshold){
  # Note: d = proportional data frame, dst = data source table, 
  # nt = # of temporal samples data frame. Each of these are
  # defined in the wrapper function below.
  # Get data:
    dataset = as.numeric(substr(site, 2, 4))
  # Subset to the site of interest:
    d = d[d$site == site,]
    nt = nt[nt$site == site,'nt']
    dst = dst[dst$dataset_ID == dataset,]
  # Calculate richness indices:
    rich.total = length(d[,1])
    rich.core = length(d[d$occ>=1-threshold,1])
    rich.trans = length(d[d$occ<=threshold,1])
  # Calculate proportional occurrences:
    prop.core = rich.core/rich.total
    prop.trans = rich.trans/rich.total
    mu = mean(d$occ)
  # Calculate bimodality of the dataset and site:
    bimodal = bimodality(d$occ, nt)
    t.out = tokeshiFun(site, threshold)
  # Output
    return(data.frame(dataset, site, threshold, system = dst$system,
              taxa = dst$taxa, N.time = nt,
              rich.total, rich.core, rich.trans, 
              prop.core, prop.trans, mu, bimodal,
              P.ct = t.out$Pc, P.core = t.out$Pr,
              P.trans = t.out$Pl))
  }

#----------------------------------------------------------------------------------*
# ---- Function to return core-transient summary analysis for all sites ----
#==================================================================================*

coreTrans = function(threshold){
  # Get data:
    d = read.csv('output/prop.df.csv')
    dst = read.csv('data_source_table.csv')
    nt = read.csv('output/Ntime.df.csv')
  # For loop inputs
  site = unique(d$site)
  out.list = list()
  for(i in site){
    out.list[[i]] = ctSummary(d, dst , nt, i, threshold)
  }
  rbind.fill(out.list)
}

#==================================================================================*
# ---- PLOT FUNCTIONS ----
#==================================================================================*

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


