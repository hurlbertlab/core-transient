library('plyr')
library('dplyr')
library('tidyr')

source('scripts/R-scripts/core-transient_functions.R')

#---------------------------------------------------------------------
# Function to create fake datasets

fakeData = function(nSites, nPlots, nYears, nDatesPerYear, 
                    startingSite = 1,
                    startingYear = 2001,
                    startingPlot = 1,
                    startingDate = 1,
                    nSpecies = 10) {
  
  sitenums = startingSite:(startingSite + nSites - 1)
  years = startingYear:(startingYear + nYears - 1)
  plots = startingPlot:(startingPlot + nPlots - 1)
  dates = startingDate:(startingDate + nDatesPerYear -1)
  sites = rep(sitenums, each = nSpecies * nPlots*nYears*nDatesPerYear)
  plot = rep(letters[plots], each = nDatesPerYear*nSpecies, times = nSites*nYears)
  
  dataOut = data.frame(site = paste(sites,plot, sep ='_'),
                       year = rep(years, each = nSpecies*nPlots*nDatesPerYear, times = nSites),
                       jd = rep(dates, each = nSpecies, times = nPlots*nYears*nSites),
                       species = rep(letters[1:nSpecies], times = nSites*nPlots*nYears*nDatesPerYear),
                       count = 1)
  dataOut$date = paste(dataOut$year, "01", dataOut$jd, sep = '-')
  return(dataOut)
}

#---------------------------------------------------------------------------------
# Assume all test datasets have subannual sampling grain, categorical and hierarchical
# spatial sampling not defined by lat-longs.
dataDescription = list(subannualTgrain = "Y",
                       spatial_scale_variable = "Y",
                       LatLong_sites = "N",
                       Raw_siteUnit = "site_plot")
                       
# Further that all datasets will be aggregated at a yearly temporal grain and
# the site spatial grain.
temporalGrain = 'year'
spatialGrain = 'site'
minNTime = 4
minSpRich = 4
proportionalThreshold = 0.5

#--------------------------------------------------------------------------------
# Evaluating test datasets:

# Dataset 1: all site-years with equal subsamples
# Expected output: w = 4, z = 4 (at larger scale)
#                  No sites dropped
testData1 = fakeData(nSites = 4, nPlots = 4, nYears = 10, nDatesPerYear = 4)


# Dataset 2: half of sites with high spatial subsampling, half with low,
#            all with equal temporal subsampling
# Expected output: w = 6, z = 4
#                  2/4 sites dropped

d2a = fakeData(nSites = 2, nPlots = 6, nYears = 10, nDatesPerYear = 4)
d2b = fakeData(nSites = 2, nPlots = 2, nYears = 10, nDatesPerYear = 4, 
               startingSite = 3)
testData2 = rbind(d2a, d2b)


# Dataset 3: half of sites with high spatial subsampling but low temporal
#            and vice versa
# Expected output: w = 6, z = 2 or
#                  w = 2, z = 6 BUT NOT
#                  w = 6, z = 6 because no sites will meet that criterion
#                  2/4 sites dropped

d3a = fakeData(nSites = 2, nPlots = 6, nYears = 10, nDatesPerYear = 2)
d3b = fakeData(nSites = 2, nPlots = 2, nYears = 10, nDatesPerYear = 6, 
               startingSite = 3)
testData3 = rbind(d3a, d3b)


# Dataset 4: half+1 of sites with high spatial subsampling but low temporal
#            and half-1 have low spatial subsampling and high temporal subsampling
# Expected output: w = 6, z = 2   since this yields more sites than w = 2, z = 6
#                  3/8 sites dropped


d4a = fakeData(nSites = 5, nPlots = 6, nYears = 10, nDatesPerYear = 2)
d4b = fakeData(nSites = 3, nPlots = 2, nYears = 10, nDatesPerYear = 6,
               startingSite = 6)

testData4 = rbind(d4a, d4b)


# Dataset 5: 1 site with high subsampling, 3 sites with low subsampling
# Expected output: w = 6, z = 6
#                  3/4 sites dropped

d5a = fakeData(nSites = 1, nPlots = 6, nYears = 10, nDatesPerYear = 6)
d5b = fakeData(nSites = 3, nPlots = 2, nYears = 10, nDatesPerYear = 2,
               startingSite = 2)
testData5 = rbind(d5a, d5b)


# Dataset 6: 1 site with high subsampling in 6 years, and low subsampling in 6 years,
#            and very low subsampling in 4 years
# Expected output: w = 2, z = 4
#                 No sites dropped, 4 years dropped

d6a = fakeData(nSites = 1, nPlots = 6, nYears = 6, nDatesPerYear = 4)
d6b = fakeData(nSites = 1, nPlots = 2, nYears = 6, nDatesPerYear = 4, 
               startingYear = 2007)
d6c = fakeData(nSites = 1, nPlots = 1, nYears = 4, nDatesPerYear = 4,
               startingYear = 2013)

testData6 = rbind(d6a, d6b, d6c)


# Dataset 7: 1 site with high subsampling in 6 years, and low subsampling in 6 years,
#            site 2 has low subsampling 
# Expected output: w = 6, z = 4
#                  16 years dropped, site 2 dropped

d7a = fakeData(nSites = 1, nPlots = 6, nYears = 10, nDatesPerYear = 4)
d7b = fakeData(nSites = 1, nPlots = 2, nYears = 10, nDatesPerYear = 4, 
               startingYear = 2007)
d7c = fakeData(nSites = 1, nPlots = 1, nYears = 10, nDatesPerYear = 4,
               startingSite = 2, startingYear = 2013)

testData7 = rbind(d7a, d7b, d7c)

# Expected values of w and z for the test datasets above:
wShouldBe = c(4, 6, 6, 6, 6, 2, 6)
zShouldBe = c(4, 4, 2, 2, 6, 4, 4)

testDatasets = ls()[grep('testData', ls())]
wResults = vector(length = length(testDatasets))
zResults = c()
for (i in 1:length(testDatasets)) {
  dataset = get(testDatasets[i])
  inData = richnessYearSubsetFun(dataset, spatialGrain, temporalGrain, minNTime, 
                                  minSpRich, dataDescription)
  
  zOut = zFinder(inData, minNTime, proportionalThreshold)
  wOut = wFinder(inData, minNTime, proportionalThreshold)
  if (zout$z == zShouldBe[i]) {
    zResults[i] = "PASS"
  } else {
    zResults[i] = "FAIL"
  }
}
#-------------------------
inData1 = richnessYearSubsetFun(testData1, spatialGrain, temporalGrain, minNTime, 
                                minSpRich, dataDescription)

zOut1 = zFinder(inData1, minNTime, proportionalThreshold)
wOut1 = wFinder(inData1, minNTime, proportionalThreshold)



