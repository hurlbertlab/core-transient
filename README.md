# Core-transient
Data and code for NSF funded research on core vs transient species

## Navigating the core-transient file system:
The **core-transient** repo includes:
  - the data_formatting_table.csv, which includes the list of datasets and their metadata
    - note that dataset ID's less than 200 were obtained from Maria Dornelas from the compilation underlying Dornelas et al. 2014, *Science* 344: 296-299, although we excluded many datasets from that compilation due to more stringent time series requirements
  - a file describing all of the fields in the data_formatting_table.csv
  - a scripts folder including dataset-specific cleaning scripts (see below)
  - an output folder that contains analytical results and data summaries of the compilation
  - a Reference folder that includes the NSF proposal and some random project notes

The project also includes a **core-transient-data** sub-repo that is private. This sub-repo contains
  - a raw_datasets folder, which includes the raw data for each dataset named using the convention 'dataset_XXX.csv' where XXX is our assigned dataset ID.
    - note that in some instances, the raw data files required a small amount of format-massaging, or pasting together from multiple raw files, in which case the truly raw data files are in a subfolder called 'dataset_XXXRAW' along with the preformatting script.
    - in that case, the dataset_XXX.csv file in the raw_datasets folder is the output of that preformatting script
  - a formatted_datasets folder, which includes data files that have been processed by their respective data cleaning scripts to have the following fields: datasetID, site, date, species, count
  - a propOcc_datasets folder which has proportional occupancy values for each species at each site within a dataset
  - a siteSummaries folder which has a summary of richness, number of temporal sampling events, and average total abundance per sampling event for each site in the dataset


## R-scripts and functions in the core-transient file system
Early attempts to automate the process of formatting the datasets were met with difficulties associated with marked differences in how the data were coded across studies. Because of this, we’ve found it necessary to format and prepare data for analysis on a dataset-by-dataset basis. In the R-scripts folder (‘core-transient/scripts/R-scripts’) you will find two files: core-transient_functions.R and a folder of data_cleaning_scripts.

  - **core-transient_functions.R** contains ALL of the functions that underly the data_cleaning_scripts. Many of the functions on this script automate the extensive processes necessary to prepare derived dataset scripts from the raw data. It is absolutely necessary that all inputs are coded correctly. Be sure to pay close attention to the data formatting and metadata tables to ensure that this is the case! At certain points, error messages will assist you in determining where you may have strayed. Additionally, this script includes functions that underly basic site-level data exploration, including:
    - Assessing bimodality
    - Determing the parameters of the beta distribution
    - Displaying summary statistics
    - Displaying a histogram
  - **data_cleaning_script**. Each data cleaning script (titled: dwork_XXX.R) is separated out into two basic parts.
    - **Data formatting**: This section takes the raw data and places fields within a format that is universal across datasets. Additionally, this process involves a considerable amount of data “cleaning”, which includes removing bad species, time records, and site designations. Another crucial component of each data formatting script are sets of lines used to update the data formatting table. This table is used for further data processing once the data formatting is complete. The data formatting process is broken into the following sections:
        - Exploring the overall data structure to familiarize yourself with the fields and current format of the raw data structure.
        - Exploring and formatting site data: Here, the data formatter will explore any available metadata associated with a dataset and determine how the researcher initially defined their sites. They will then arrange any nested sites in the order from largest to smallest site grain (e.g., region - site - plot -quad).
        - Exploring and formatting species data: This task predominantly includes removing bad species (e.g., “bare ground” or “unknown”) and fixing mispellings or inconsistent capitalization in species names.
        - Exploring and formatting time data: All time data are formatted either as year (if this is the finest available temporal grain or as a POSIX date object)
    - Making **proportional occurrence** and **site summary** frames: Here, the data formatter will need to further explore potential scale issues in order to make proportional occurrence data frames (and summary files) that reflect as little sampling bias as possible. This first involves testing whether there the species richness and number of time samples at a given set is above a user defined theshold. If this is not the case, the user may have to vary the temporal or spatial grain of the analysis, if this option is available. The script then uses a function in the core-transient-functions folder to subset the data such that all sites meet the species richness and time sample criteria as well subsamples (in terms of spatially and/or temporally nested data) each site to ensure that sampling intensity is equivalent across all site-years. Once this occurs, the data formatter runs canned functions that create and write both the proportional occurrence and site summary datasets.

## Tests
A test of the functions which decide what levels of spatial and temporal subsampling to use (if any) can be run in R by typing:
```
source('scripts/R-scripts/tests/data_subsampling_test.R')
```
This should be run any time any of the functions in 'core-transient_functions.R' are modified to ensure that data is still being subsampled as expected. This set of tests may not be exhaustive...
    
## Project status

- **Data Preparation functions and scripts**: All data preparation scripts have been tested. The data formatting is working as expected as is the creation of proportional occurrence and site summary frames. _**Still Needed**_:  A modification should be made to the data formatting script to explore the number of time samples (evaluated as years) per site as a component of site exploration. Formatting on a given dataset should be put on hold if the number of time samples is inadequate (greater than or equal to 10 is the current cut-off). See processed datasets and current priority rankings below.

- **Data analysis functions and scripts**: Statistical summaries include: species richness of core and transient species, proportional core and transient species, p-value of core- and transient proportions, mu, bimodality, p-value of bimodality, and beta distribution parameters. Statistical summaries have been tested and are working on the newly prepared datasets. The one current plotting function, the histogram, is working. _**Still needed**_: A previous iteration of the analysis component included a "data analysis dashboard" from which statistical analyses were run. The function to loop analyses (including statistic summaries and plotting) across unanalyzed data is in place, however, the recreating the script for running the loop is a necessary step.

- **Dataset summaries tables**: The metadata and data formatting tables have been updated to the relevant fields and most current information. _**Still needed**_: The metadata table, as well as the data formatting table still needs to be updated with information on the individual datasets. This should be a high priority task, especially in terms of providing links to online metadata for a given study.

- **Processed datasets**: Currently, there are 12 datasets processed with proportional occurrence frames and 33 formatted datasets. The disparity between the number of proportional occurrence frames and formatted datasets predominantly reflects datasets with an inadequate number of time samples for each site given the current cut-off value of 10. All datasets for which proportional occurrence frames can be constructed under current cut-offs have been made. _**Still needed**_: To obtain more proportional occurrence frames under the current cut-off values, I suggest only formatting new datasets with at least 10 years of data (per site).

## *Format flag codes*

 0 = not yet examined  
 1 = formatting complete  
 2 = formatting in process  
 3 = formatting halted, issue  
 4 = data unavailable  
 5 = data inadequate for project aims  


