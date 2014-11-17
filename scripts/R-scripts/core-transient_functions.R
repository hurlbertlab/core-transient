###################################################################################*
# CORE-TRANSIENT FUNCTIONS                                                         *
###################################################################################*
# This script contains all of the functions used in the analyses that summarize
# core-transient data by site (and across sites). It is divided in 3 parts:
#   1. Bimodality summary statistics
#   2. Summary output tables for a given site and across sites
#   3. Plot output

#==================================================================================*
# ---- BIMODALILITY ----
#==================================================================================*
# NOTE: For these functions to run, prop.df, Ntime, and outSummary frames must
# already be loaded and the "Sampling summary" lines of code MUST be run in the 
# dashboard!
#
# Functions:
# - bimodality: Calculates the bimodality metric developed by Allen and Ethan. 
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - random.bimodality: The bimodality for a random sample of the dataset.
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - p.bimodal: Randomization test for bimodality. Runs n-reps of the random.bimodality 
#     function and compares the actual bimodality with the distribution of random 
#     values.
#     Inputs: Site, number of reps
#     Outputs: A single numeric p-value.
#
# - occs.scaled: Scales occupancy from [0,1] to (0,1) -- because beta distribution
#     inputs must not contain 0's or 1's.
#     Inputs: Site
#     Outputs: A numeric vector of scaled occupancy values.
# 
# - fitbeta: Calculates the shape parameters for a fitted beta distribution.
#     Inputs: Site
#     Outputs: A vector of shape parameters (alpha and beta).
# 
#----------------------------------------------------------------------------------*
# ---- Function for calculating bimodality ----
#==================================================================================*
# Note: bimodality is the fraction of species occurring at either end of occupancy
# distribution. We use a randomization approach to test whether the distribution
# is significantly bimodal.

# True bimodality for a given site:

bimodality = function(site) {
  occs = prop.df[prop.df$site == site,'occ']             # Get occurence data for site
  n.time = nTime[nTime$site == site,'nt']               # Get # of years for site
  maxvar = var(c(rep(1/n.time,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

# Random bimodalility for a given site (to be used in randomization, below):

random.bimodality = function(site){
  # Set-up:
  df = prop.df[prop.df$site == site,]               # Get occurence data for site
  nt = nTime[nTime$site == site,'nt']               # Get # of years for site
  t1 = data.frame(table(df$occ))                    # Occurence proportion and frequency
  occ = data.frame(occ = seq(1/nt, 1, length = nt)) # Possible occurence proportions
  t2 = merge(occ, t1, by.x = 'occ',by.y = 'Var1', all.x = T)  # Occurence by possible proportions
  t2[is.na(t2[,2]),2]<-0                            # Replace NA's with zeros
  # Reassign bin values randomly and add to frame:
  new.freq = sample(t2$Freq, length(t2[,1]))
  t3 = data.frame(t2[,1], new.freq)
  # Create new occurence vector:
  occs=unlist(apply(t3, 1, function(x) rep(x[1], x[2])))
  occs = as.vector(occs)
  # Calculate bimodality:
  maxvar = var(c(rep(1/nt,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

# Randomization test for bimodality:

p.bimodal = function(site, reps){
  nt = nTime[nTime$site == site,'nt']               
  actual.bimod = bimodality(site)
  # For loop to get random bimodality values
  r.bimod = numeric()
  for (i in 1:reps){
    r.bimod[i] = random.bimodality(site)
  }
  # Calculate the p-value (proportion of sites with higher bimodality than the
  # actual bimodality value):
  sum(r.bimod >= actual.bimod)/(reps + 1)
}

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
# Required packages = MASS

# Scale occupancy from [0,1] to (0,1) following Smithson and Verkuilen 2006
# Note: See supplemental at
# http://supp.apa.org/psycarticles/supplemental/met_11_1_54/met_11_1_54_supp.html

occs.scaled = function(site){
  x = prop.df[prop.df$site == site,'occ']
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
}

# Fit beta distribution:

fitBeta = function(site) {
  if (bimodality(site)!= 0) {
  occs  = occs.scaled(site)
  shape.params = suppressWarnings(fitdistr(occs, "beta",
                                  list(shape1 = 2, shape2 = 2)))
  return(as.vector(shape.params$estimate))
  } else c(NA, NA)
}

#==================================================================================*
# ---- DATASET SUMMARY FUNCTIONS ----
#==================================================================================*
# NOTE: For these functions to run, prop.df, Ntime, and outSummary frames must
# already be loaded!
#
# Functions:
# - sampling: Produces summary sampling data for one site. 
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with dataset ID, site ID, threshold used,
#       the system, taxa, # of time samples, total, core, and transient richness
#       proportion of core and transient species, and the average proportion of 
#       occurance across species.
#
# - ctSummary: A partial-wrapper function that runs and compiles bimodality test
#     statistics across sites and adds it to the sampling summary frame above.
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with the summary output above and bimodality
#     (Allen + Ethan formula), randomization-derived p-value, and the alpha and
#     beta shape parameters for the beta distibution.
#
#----------------------------------------------------------------------------------*
# ---- Function to generate summary of sampling ----
#==================================================================================*

sampling = function(site, threshold){
  # Get data:
  dataset = as.numeric(substr(site, 2, 4))
  d = prop.df[prop.df$site == site,]
  nTime = nTime[nTime$site == site,'nt']
  dst = outSummary[outSummary$dataset_ID == dataset,]
  # Subset to the site of interest:
  d = d[d$site == site,]
  dst = dst[dst$dataset_ID == dataset,]
  # Calculate richness indices:
  rich.total = length(d[,1])
  rich.core = length(d[d$occ>=1-threshold,1])
  rich.trans = length(d[d$occ<=threshold,1])
  # Calculate proportional occurrences:
  prop.core = rich.core/rich.total
  prop.trans = rich.trans/rich.total
  mu = mean(d$occ)
  # Output 1-row dataset:
  return(data.frame(dataset, site, system = dst$system, taxa = dst$taxa, nTime,
                    rich.total, rich.core, rich.trans, prop.core, prop.trans, mu))
}

#----------------------------------------------------------------------------------*
# ---- Function to generate output summary dataset ----
#==================================================================================*

ctSummary = function(site, threshold, reps){
  # Get data:
    dataset = as.numeric(substr(site, 2, 4))
  # Subset to the site of interest:
    d = prop.df[prop.df$site == site,]
    nt = nTime[nTime$site == site,'nt']
    dst = outSummary[outSummary$dataset_ID == dataset,]
  # Sampling summary for site:
    samplingSummary = sampling(site, threshold)
  # Calculate bimodality of the dataset and site:
    bimodal = bimodality(site)
    bimodal.p = p.bimodal(site, reps)
  # Calculate the alpha and beta shape parameters for the beta disatribution:
    fB = fitBeta(site)
  # Make a bimodality dataframe:
    bimodality.df = data.frame(bimodal, bimodal.p, alpha = fB[1], beta = fB[2])
  # Output
    return(cbind(samplingSummary, bimodality.df))
  }

#==================================================================================*
# ---- PLOT FUNCTIONS ----
#==================================================================================*
# NOTE: For these functions to run, prop.df, Ntime, and outSummary frames must
# already be loaded!
#
#----------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#==================================================================================*
# This function creates a ct histogram for one site:

ct.hist = function(site) {
  # Get data, subset to a given site:
      prop.df = prop.df[prop.df$site == site,]
      ct = ct[ct$site == site, ]
  # Plot labels:
    main = paste('Site ', site, paste('(',  as.character(ct$system),
                   ', ', as.character(ct$taxa),')', sep = ''))
    sub = bquote(b ~ '=' ~ .(round(ct$bimodal, 2)) ~ '    '~
                   P['b'] ~ '=' ~ .(round(ct$bimodal.p, 3)) ~ '    '~
                   mu ~ '=' ~ .(round(ct$mu, 2)) ~ '    '~
                   t ~ '=' ~ .(ct$nTime))
    sub2 = bquote(alpha ~ '=' ~ .(round(ct$alpha, 3)) ~ '    '~
                   beta ~ '=' ~ .(round(ct$beta, 3)))
  # Set band width, breaks and possible values of x for the histogram:
    bw = (max(prop.df$occ)-min(prop.df$occ))/10
    brks = seq(min(prop.df$occ), max(prop.df$occ),bw)
    x = seq(0.01,.99, .01)
  # Plot data: 
    out.plot = ggplot(prop.df, aes(x=occ)) +
      geom_histogram(aes(y = ..density..), breaks = brks, right = F,
                     fill = 'gray', color = 1) +
      geom_density(alpha=.2, fill="blue") +  
      stat_function(fun = function(x) dbeta(x, ct$alpha, ct$beta), color = 'red') +
      # Add labels:
      xlab('Proportion of temporal samples') + ylab('Density') + 
      ggtitle(bquote(atop(.(main), atop(.(sub), atop(.(sub2)))))) +
      # Add themes:
      theme(axis.text = element_text(size=14, color = 1),
            axis.title.x = element_text(vjust = -1),
            axis.title.y = element_text(vjust = 2),
            title = element_text(size=18, vjust = -1),
            axis.line = element_line(colour = "black"),
            panel.background = element_blank(),
            plot.margin = unit(c(.5,.5,1.5,1), "lines"))
    return(out.plot)
  }
