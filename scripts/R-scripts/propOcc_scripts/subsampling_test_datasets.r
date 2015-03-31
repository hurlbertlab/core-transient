# Test datasets for subsampling function

# Function to create fake datasets

fakeData = function(nSites, nPlots, nYears, nSeasons, 
                    startingSite = 1,
                    startingYear = 2001,
                    startingPlot = 1,
                    startingSeason = 1) {
  
  sitenums = startingSite:(startingSite + nSites - 1)
  years = startingYear:(startingYear + nYears - 1)
  plots = startingPlot:(startingPlot + nPlots - 1)
  seasons = startingSeason:(startingSeason + nSeasons -1)
  
  dataOut = data.frame(site = rep(sitenums, each = nPlots*nYears*nSeasons), 
                       plot = rep(letters[plots], each = nSeasons, times = nSites*nYears),
                       year = rep(years, each = nPlots*nSeasons, times = nSites),
                       season = rep(seasons, times = nPlots*nYears*nSites))
  return(dataOut)
}

# These datasets should be tested by finding the combination of w
# (spatial subsampling) and z (temporal subsampling) that reflect
# the top 25% (since fake datasets are in units of 4) of
# subsampling intensity


# Dataset 1: all site-years with equal subsamples
# Expected output: w = 4, z = 4
#                  No sites dropped

d1 = fakeData(nSites = 4, nPlots = 4, nYears = 4, nSeasons = 4)


# Dataset 2: half of sites with high spatial subsampling, half with low,
#            all with equal temporal subsampling
# Expected output: w = 6, z = 4
#                  2/4 sites dropped

d2a = fakeData(nSites = 2, nPlots = 6, nYears = 4, nSeasons = 4)
d2b = fakeData(nSites = 2, nPlots = 2, nYears = 4, nSeasons = 4, 
               startingSite = 3)
d2 = rbind(d2a, d2b)

# Dataset 3: half of sites with high spatial subsampling but low temporal
#            and vice versa
# Expected output: w = 6, z = 2 or
#                  w = 2, z = 6 BUT NOT
#                  w = 6, z = 6 because so sites will meet that criterion
#                  2/4 sites dropped

d3a = fakeData(nSites = 2, nPlots = 6, nYears = 4, nSeasons = 2)
d3b = fakeData(nSites = 2, nPlots = 2, nYears = 4, nSeasons = 6, 
               startingSite = 3)
d3 = rbind(d3a, d3b)


# Dataset 4: half+1 of sites with high spatial subsampling but low temporal
#            and half-1 have low spatial subsampling and high temporal subsampling
# Expected output: w = 6, z = 2   since this yields more sites than w = 2, z = 6
#                  3/8 sites dropped


d4a = fakeData(nSites = 5, nPlots = 6, nYears = 4, nSeasons = 2)
d4b = fakeData(nSites = 3, nPlots = 2, nYears = 4, nSeasons = 6,
               startingSite = 6)
d4 = rbind(d4a, d4b)


# Dataset 5: 1 site with high subsampling, 3 sites with low subsampling
# Expected output: w = 6, z = 6
#                  3/4 sites dropped

d5a = fakeData(nSites = 1, nPlots = 6, nYears = 4, nSeasons = 6)
d5b = fakeData(nSites = 3, nPlots = 2, nYears = 4, nSeasons = 2,
               startingSite = 2)
d5 = rbind(d5a, d5b)


# Dataset 6: 1 site with high subsampling in 6 years, and low subsampling in 6 years,
#            and very low subsampling in 4 years
# Expected output: w = 2, z = 4
#                  4/16 years dropped

d6a = fakeData(nSites = 1, nPlots = 6, nYears = 6, nSeasons = 4)
d6b = fakeData(nSites = 1, nPlots = 2, nYears = 6, nSeasons = 4, 
               startingYear = 2007)
d6c = fakeData(nSites = 1, nPlots = 1, nYears = 4, nSeasons = 4,
               startingYear = 2013)

d6 = rbind(d6a, d6b, d6c)
