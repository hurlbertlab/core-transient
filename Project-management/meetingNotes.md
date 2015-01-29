---

## Meeting notes: Meeting with Allen, 10/2/14
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

---

## Meeting on 10/9 with Ethan and Allen
* We've switched from https to ssh. See help on github for generating ssh keys (was thankfully easy on Mint!)

### Working with the submodule
* Added submodule "data" which contains the core-transient datasets folder.
* Make sure to use the following commands to work with the submodule on a new machine:
* git submodule init
* git submodule update
* From now on, work with data within the submodule rather than the core-transient-datasets folder
* add-commit-pull will now have an additional step, once per day you have to commit and push the submodule:
git add data
git commit -m "daily update of the data file"
_**Note:** this allows the changes to be known across machines_

---

## Meeting notes, Ethan and Allen 10/23

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
