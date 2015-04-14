
#-------------------------------------------------------------------------------*
# ---- SUBSET DATASET TO SITES WITH ADEQUATE TIME SAMPLES AND RICHNESS ----
#-------------------------------------------------------------------------------*

# Note: Prior to running "RichnessYearSubsetFrame", you must have created the nestedDataset using the function "getNestedDataset". The output of the getNestedDataset function is a list with the first list item being the dataset, expanded across all potential spatial and temporal grains and the second being a vector of the names of the site columns. The "i" in this function refers to the the value in the vector of site names. 

RichnessYearSubsetFrame = function(i, minNYears = 10, minSpRich = 10){
  # Get the name of the site column at a given spatial grain:
    siteID = nestedDataset[[2]][i]
  # Extract the nested dataset:
    nestedDatasetDf = nestedDataset[[1]]
  # Add a column named siteID:
    nestedDatasetDf$siteID = nestedDatasetDf[,siteID]
  # Get the number of years and species richness for each site: 
    siteSr_nTime = ddply(nestedDatasetDf, .(siteID), summarize,
                         sr = length(unique(species)), 
                         nTime = length(unique(year)))
  # Subset to sites with a high enough species richness and year samples:
    goodSites = filter(siteSr_nTime, sr >= minSpRich & 
                         siteSr_nTime$nTime >= minNYears)$siteID
  # Match good sites and the dataframe:
    outFrame = na.omit(nestedDatasetDf[nestedDatasetDf$siteID %in% goodSites,])
  # Return output (note: "select(one_of" returns just the specified columns)
    return(select(outFrame,
                  one_of(c('siteID','date','year', 'site', 'species','count'))))
}

#-------------------------------------------------------------------------------*
# ---- CALCULATE the Z-threshold ----
#-------------------------------------------------------------------------------*
# The Z-threshold refers to the maximum number of temporal subsamples that provide the most sites with greater than a minimum number of years of data. The following function returns this value.

# Note: Prior to running "zFinder", you must have already run the function "RichnessYearSubsetFrame" for which "inData" is the function's output.  

zFinder = function(inData, minNYears = 10, proportionalThreshold = .5){
  data = inData  
  # Calculate the number of temporal samples per site and year: 
    spaceTime = ddply(data, .(siteID, year),
                      summarize, temporalSubsamples = length(unique(date)))
  
  # zPossible is a potential threshold of temporal subsampling:
    zPossible = sort(unique(spaceTime$temporalSubsamples))

  # Create an empty matrix to store summary data for possible Z-values:
    zMatrix = matrix(ncol = 3, nrow = length(zPossible), 
                     dimnames = list(NULL, c('z','nSites','propSites')))

  # Create an empty list of sites to store site names of good sites at a given Z-value:
    zSiteList = list(length = length(zPossible))

  # For loop to populate the zMatrix and zSite Lists:
  
    for(i in 1:length(zPossible)){
      # Calculate the years in which the subsamplings was greater than equal to z for a given site:
        siteYearsGTEz = ddply(filter(spaceTime, temporalSubsamples>=zPossible[i]),
                          .(siteID), summarize, 
                          yearsGTEz = length(unique(year)))
      # Determine sites with a minimum number of 10 years of sampling >= z
        goodSites = filter(siteYearsGTEz, yearsGTEz>=minNYears)$siteID
      # Construct matrix of z values, the number and proportion of sites:
        zMatrix[i,'z'] = zPossible[i]
        zMatrix[i, 'nSites'] = length(goodSites)
        zMatrix[i, 'propSites'] = length(goodSites)/length(unique(data$siteID))
      # List the names of goodSites for a given Z-value:
        zSiteList[[i]] = goodSites
      # Name each list entry by the Z-value
        names(zSiteList)[[i]] = zPossible[i]
      }

  # Get the highest Z value with at least minNYears:
    z = max(filter(data.frame(zMatrix), propSites >= proportionalThreshold)$z)
  
  # Get the names of the sites that satisfy Z:
   zSites = factor(zSiteList[[as.character(z)]])

  # Return the z value and site names 
    return(list (z = z, zSites = zSites))
}

inData = RichnessYearSubsetFrame(1)

zOutput = zFinder(inData)

#-------------------------------------------------------------------------------*
# ---- CALCULATE the W-threshold, subset data ----
#-------------------------------------------------------------------------------*
# The W-threshold refers to the maximum number of spatial subsamples that provide a given proprtion of siteYears.

# Note: Prior to running "wzDataSubset", you must have already run the function "RichnessYearSubsetFrame" (inData) and the "zFinder" (zOutput).  

wzDataSubset = function(inData, zOutput, minNYears = 10, proportionalThreshold = .5){
  data = inData
  z = zOutput[[1]]
  # Subset data to just the sites that meet the z-threshold:
    dataZ = filter(data, siteID %in% zOutput$zSites)
  # Add a siteYear column:
    dataZ$siteYear = paste(dataZ$siteID, dataZ$year, sep ='_')
  # Summarize data by the number of temporal samples in a given year:
    spaceTime = ddply(dataZ, .(siteYear), summarize, 
                           temporalSubsamples = length(unique(date)),
                           spatialSubsamples = length(unique(site)))
  
  # Subset spaceTime to years with >= z temporal samples
   spaceTimeZsub = filter(spaceTime, temporalSubsamples>=z)
  
  # Determine the number of siteYears present:
    nSiteYears = nrow(spaceTimeZsub)
  
  # Get possible values for w:
    wPossible = sort(unique(spaceTimeZsub$spatialSubsamples))
  
  # Create an empty matrix to store summary data for possible W-values:
    wMatrix = matrix(ncol = 3, nrow = length(wPossible), 
                   dimnames = list(NULL, c('w','nSiteYears','propSiteYears')))
  
  # Create an empty list of sites to store site names of good sites at a given W-value:
    wSiteYearList = list(length = length(wPossible))
  
  # For loop to populate the wMatrix and wSite Lists:
  
    for(i in 1:length(wPossible)){
      # Calculate the years in which the subsamplings was greater than equal to w for a given site:
        siteYearsGTEw = filter(spaceTimeZsub, spatialSubsamples>=wPossible[i])$siteYear
      # Construct matrix of w values, the number and proportion of sites:
        wMatrix[i,'w'] = wPossible[i]
        wMatrix[i, 'nSiteYears'] = length(siteYearsGTEw)
        wMatrix[i, 'propSiteYears'] = length(siteYearsGTEw)/nrow(spaceTimeZsub)
      # List the names of siteYears for a given W-value:
      wSiteYearList[[i]] = siteYearsGTEw
      # Name each list entry by the Z-value
      names(wSiteYearList)[[i]] = wPossible[i]
    }
  
  # Get the highest W value with at least minNYears:
    w = max(filter(data.frame(wMatrix), propSiteYears >= proportionalThreshold)$w)
  
  # Get the names of the siteYears that satisfy W:
    wSiteYears = factor(wSiteYearList[[as.character(w)]])
  
  # Subset data
  
  dataW = filter(dataZ, siteYear %in% wSiteYears)
  
  
  # Return the w,z values and dataframe
  
  return(list (w = w, z = z, data = dataW))
}

  

  
  
  
  
  