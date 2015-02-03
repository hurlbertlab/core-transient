# Meeting notes

***

## Meeting notes: Allen, 7/15/14 

* Overall purpose: To lay out short term goals for the coming few weeks and determine how to deal with some problems in datasets.
* Goal: Start working on the beginnings of the analysis script. Use 3-4 datasets that pose diverse "problems" in doing so. Try to make the process as generalizeable as possible and get the work flow for the analysis down.
* Goal: In the data_source_table, possibly create a column that describes whether a data set needs to be re-organized or cleaned before it can be run. As most datasets likely need some sort of re-organization, this column may be unnecessary.
* Goal: Create a folder of R-scripts that are used for re-organizing or cleaning data. This process should be automated and be as generalizable as possible (i.e., try to write scripts that can be used for as many datasets as possible). Also create a script that hunts for mispellings.
* Goal[ignore for now]: We will need to clean the taxonomic names. The R package taxize may be useful for this.
* Goal: Spatial grain should be a quantitative column rather than categorical. Categorical descriptions of the spatial grain (and extent) can be derived from this value.
* Goal: Create a folder in the core-transient-datasets git where data papers associated with the datasets are stored.
* Goal: Add columns to the data_source_table that describe whether a dataset is suitable for goals 2 and 3 of the project
 * 2: Sites must contain latlongs and have >= 10 sites (currently, the latter may change).
 * 3: Sites must be able to be nested at larger spatial scales.
* General notes:
 * Because of siteID and x,y coordinate problems, we will wait until late August for the Dornelas data, taking her up on her offer to run the queries for us.
 * Aggregating spatial and temporal data may be an issue (i.e., choosing the appropriate scales by which to aggregate). We will cross this bridge later.

***

## Meeting notes: Allen, 10/2/14
* make ~30 commits per day, each commit should contain only one chunk of information (i.e. refer to a change in only one file)
* make use of bioark because it’s backed up every night!
* email Maria re having her assistant query the data
* Post issues to self as a to-do list
* add and modify code from Coyle et al. (week 2)
* continue to clean and format data files (week 1)
* What the heck are submodules and can they be used to avoid the setwd problem?
* Remove setwd from scripts
* We need to come up with a way to prioritize dataset so that we are ensuring that we get an adequate grab of taxa and regions in the preliminary analyses.
* For d228, add a comment in the script that says that each text file is representative of one of 4 hubbard brook sites

***

## Meeting notes: Ethan and Allen, 10/9/14
* We've switched from https to ssh. See help on github for generating ssh keys (was thankfully easy on Mint!)

### Working with the submodule
* Added submodule "data" which contains the core-transient datasets folder.
* Make sure to use the following commands to work with the submodule on a new machine:
* git submodule init
* git submodule update
* From now on, work with data within the submodule rather than the core-transient-datasets folder
* add-commit-pull will now have an additional step, once per day you have to commit and push the submodule:
```
git add data
git commit -m "daily update of the data file"
```

_**Note:** this allows the changes to be known across machines_

***

## Meeting notes: Ethan and Allen, 10/23/14

### Bimodality:
* Function needs to be modified, for max 1 represented 1/2 the time and minimum possible = 1/# of years sampled.
* Conduct a statistical test for bimodality, potentially look into the clustering world for this (see Tokeshi 1992 and Silverman 1981 papers for another option).
* Search into Gaussian mixture modeling (though this approach may have limitations)
* Look into including fitted beta parameters as a next step

### Site subsets:
Ensure that there is at least 5 samples (temporally) per site. Those that do not have this should be discarded.

### Plots:
* Include the length of the time series in the title
* Set bins such that each bin covers a 1/t block, # of bins should be # of years - 1 (?)
* Top bar shouldn't be 1 or greater, rather ensure that the top bar ends at 1
* Distribution of work: 4 categories at this point
  * a) Adding datasets
  * b) Cleaning datasets for analysis (for a and b prioritize to maximize the # of sites per dataset)
  * c) Continue work on data summary script
  * d) Work towards the inclusion of environmental variables (hold off on this for now though)

### Site table:
Include biome ... to do this, probably a gis extraction based on point data for each site.

### Environment:
* There will have to be some sort of "if" statement for extracting data, as marine and terrestrial will have different data needs
* Look into MODIS package (Allen sent email regarding this).
* We will cover thinking through environmental datasets during our next meeting (10/30)

***
## Meeting notes: Ethan and Allen, 1/26/15

* Convert the dataset processing instructions from the current .docx to markdown
* Tell Michael to include comments at the end of the script that describes any extraordinary steps taken or modifications outside of the regular template file
* Continue to chip away at the data_source table
* Finish the metadata table this sweek
* Email Zach re dataset 200 (OBIS) ... _Ethan will put us in email contact_
* Figure out the problem with the sub-repository.
 * Step 1: go through and look at the folders and contents of the repositories -- are they different? Are they split between the two?
 * See what I can do to ensure things are in the right place and not duplicated. Problems should be assigned to Ethan as an issue.
 * The data repository should contain NO scripts, just data
 * Likewise, the public repository should contain no data!
 * **IMPORTANT!!!** If there are private data outside of the data folder, these MUST be put in the data folder immediately and moved from the public repository!!!
* Clean the repository making sure to remove the junk
* Change the format priority
 * remove the # of time series from the ranking
 * change the time threshold to 9
 * 0, 1, or 2: datasets with one site (0), datasets with between 2 and 19 sites (1), datasets with 20 or more sites(1)
 * _Note: the number of sites can be a rough estimate for now._
* Fix the NA's associated with the number of sites (likely change to FILL for now, then fill them!)
* Hold off on obtaining or formatting more OBIS data -- this will be Zach's realm
* Kellog dataset -- get the data associated with it.
* Working with the ct_prop_frame 
 * Move the functions in ct_prop_frame to core-transient functions
 * Change the function ct_prop_maker to make it run more efficiently:
 * d1 does not need to be assigned (this will cut the RAM usage by 50%!)
  * just stick the subsetting right into the expression
  * by allocating the length of the list in advance it will save another 50% of RAM usage
  `list(length = length(sites))`
 * change the name "d" to focal dataset
 * look through code to find what I might be able to simplify
* try to make the code more readable by using common words for variable names and datasets -- the closer it reads to english the easier it is to understand!
* the subset function is more readable than `d[d$variable == value,]`

#### LOOKING AHEAD (long term) -> the 1st paper
* Address spatial and temporal scale (Note: geographic and latitudinal variation will not be addressed here, it will be a latter paper)k
* What figures do we want?
 1. Summary figs (we should address what these are in our next meeting
 2. Strength of evidence for bimodality
 3. Course groups and addressing the # of individuals (Note: we should discuss this further, I think my brain shut off here)

