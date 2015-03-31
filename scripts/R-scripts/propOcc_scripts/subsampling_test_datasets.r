# Test datasets for subsampling function

# Function to create fake datasets

fakeData = function(nSites, nPlots, nYears, nSeasons, siteStartingNumber = 1) {
  sitenums = siteStartingNumber:(siteStartingNumber + nSites - 1)
  dataOut = data.frame(site = rep(sitenums, each = nPlots*nYears*nSeasons), 
                       plot = rep(letters[1:nPlots], each = nSeasons, times = nSites*nYears),
                       year = rep(2001:(2001 + nYears - 1), each = nPlots*nSeasons, times = nSites),
                       season = rep(1:nSeasons, times = nPlots*nYears*nSites))
  return(dataOut)
}

# These datasets should be tested by finding the combination of w
# (spatial subsampling) and z (temporal subsampling) that reflect
# the top 75% (since fake datasets are in units of 4) of
# subsampling intensity


# Dataset 1: all site-years with equal subsamples
# Expected output: w = 4, z = 4

d1 = fakeData(nSites = 4, nPlots = 4, nYears = 4, nSeasons = 4)


# Dataset 2: half of sites with high spatial subsampling, half with low,
#            all with equal temporal subsampling
# Expected output: w = 6, z = 4

d2a = fakeData(nSites = 2, nPlots = 6, nYears = 4, nSeasons = 4)
d2b = fakeData(nSites = 2, nPlots = 2, nYears = 4, nSeasons = 4, 
               siteStartingNumber = 3)
d2 = rbind(d2a, d2b)

# Dataset 3: half of sites with high spatial subsampling but low temporal
#            and vice versa
# Expected output: w = 6, z = 2 or
#                  w = 2, z = 6 BUT NOT
#                  w = 6, z = 6 because so sites will meet that criterion


d3a = fakeData(nSites = 2, nPlots = 6, nYears = 4, nSeasons = 2)
d3b = fakeData(nSites = 2, nPlots = 2, nYears = 4, nSeasons = 6, 
               siteStartingNumber = 3)
d3 = rbind(d3a, d3b)


# Dataset 4: half+1 of sites with high spatial subsampling but low temporal
#            and half-1 have low spatial subsampling and high temporal subsampling
# Expected output: w = 6, z = 2   since this yields more sites than w = 2, z = 6


d4a = fakeData(nSites = 5, nPlots = 6, nYears = 4, nSeasons = 2)
d4b = fakeData(nSites = 3, nPlots = 2, nYears = 4, nSeasons = 6,
               siteStartingNumber = 6)
d4 = rbind(d4a, d4b)



