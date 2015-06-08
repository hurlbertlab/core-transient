**Section Two** provides instructions on how to make a proportional occurrence data frame after summarizing the data at a given spatial or temporal scale.

## SECTION TWO: CREATING PROPORTIONAL OCCURRENCE AND DATASET SUMMARY DATA FRAMES
		
We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. We've alreay done the heavy lifting of dataset formatiting and it's now to make some scale decisions and determine the proportional occupancies. To do so, we need to explore the dataset and look through the metadata for clues to the approriate temporal and spatial grain. As you create the proportional occurrence frame, be sure to keep careful notes describing how the data were summarized, including any decision made for scaling decisions.

### Temporal data:

We start by extracting year from the dataset. Year are our default temporal grain. Decisions for finer temporal grains may be decided at a later date. This process involves changing the date column to year (uses a custom function in the core-transient_functions.R file) and then renaming the column to "year".

```
dataset$date = getYear(dataset$date)

names(dataset)[3] = 'year'
```

### Site data:

Determining the appropriate spatial sampling grain can be especially challenging. If the definition of sites is not very clear cut, you will have to return to the metadata to see if there's any clues.

A quick way to determine if the sites, as defined in the formatted dataset, are adequate for our needs is to take a look at the number of time samples and species recorded at a site. We're using a cut-off of 5 sampling intervals and at least 10 observed species.

```
siteTable = ddply(dataset, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

# Sort the site table for a closer look:

head(siteTable[order(siteTable$nSp),],20)

```

In the eample dataset, several of the sites had species richness values below the cut-off (about a third of them). Sites in this study were in a nested design (e.g., quadrats within plots).  This suggests that the sampling grain is too fine. The descriptors of site in the column are separated by an underscore, in the order of the largest to smallest sampling class. In this instance, we remove the smallest category (quadrats) and explore the data to see if the new definition of a site is adequate.

We start by splitting site in separate fields in a table:

```
site = read.table(text = as.character(dataset$site), sep ='_')

head(site)
```

Then paste together all but the last site descriptor:

```
site1 = do.call('paste', c(site[,1:4],sep = '_'))
```

Then explore number of species based on the new site descriptions:

```
siteTable = ddply(dataset1, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

head(siteTable)

summary(siteTable)

head(siteTable[order(siteTable$nSp),],10)
```

In this instance we have lost only a few sites. We have to remove the bad sites prior to making the proportional occurrence data frame. We do so by defining the "bad" sites in the dataset. and then subsetting the dataframe to only the "good"sites. As always, we explore the resulting data frame and, if it is acceptable we can reassign our chosen name "dataset".

```
badSites = subset(siteSummaryFun(dataset), spRich < 10 | nTime < 5)$site

dataset1 = dataset[!dataset$site %in% badSites,]

```

Summarize the dataset to the new spatial grain and explore:

```
dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

head(dataset2)

dim(dataset2)

summary(dataset2)

dataset = dataset2
```
### Making the proportional occurrence data frame:

If the species and time samples are adequate, you are now ready to create and write the proportional occurrence data frame. This part of the process is easy becuase there is a function called PropOccFun that is located in the core-transient-functions script that does this automatically for you. Simply run the function and write the output to the file in one step:

```
write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_223.csv", row.names = F)
```

We will also write a summary of the file, that provides the number of time samples and species richness for each site using the function siteSummaryFun (also located in the core-transient-functions R script):

```
write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_223.csv', row.names = F)
