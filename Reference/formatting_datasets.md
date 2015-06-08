# Preparing datasets for analysis 

In this project, we evaluate the proportion of core and transient species at a given site using data collected across a wide range of taxa, regions, and environmental systems. A challenge that we face is that ecological data are collected at highly variable spatial, temporal, and biological scales and it is necessary to consistently format the data in preparation for analysis. Here we provide instructions associated with formatting datasets and then creating a proportional occurrence and dataset summary files. This document is divided into three parts: 
1. Instructions on how to format a raw dataset to a consistent set of fields at the finest available spatial and temporal grain. 
2. Instructions on how to construct a data frame of the proportion of time samples a species was observed at a given sign (herein this is called the proportional occurrence data frame or "_PropOcc_" for short. 
3. An "R Cheatsheet" that provides a list of the various functions you will likely use in the preparation of data for analysis.

## Dataset formatting

### OVERVIEW
In the formatting process, our goal is to ensure that the fields are all consistently of the same structure, that only valid species are listed, and count data are summarized by site, date, and species. We will avoid making any decisions on scale and, rather, provide a formatted dataset at the finest temporal and spatial scale available.

The overall goal in this step of the process is to format a dataset as follows:

datasetID | site | species | date | count
--------- | ---- | ------- | ---- | -----
01 | d01_Treatment1PlotA | A._schoenobaenus | 1928 | 1
01 | d01_Treatment1PlotA | Buteo_buteo | 1928 | 3
01 | d01_Treatment1PlotB | Corvus_corax | 1929 | 7
01 | d01_Treatment2PlotA | Cuculus_canorus | 1929 | 1
01 | d01_Treatment2PlotB | Gallinula_chloropus | 1928 | 2


Below are the steps that you should take when exploring and formatting datasets. These steps should be followed in the order that they’re presented.  _**IMPORTANT**: throughout this process, if there are any problems that keep you from successfully formatting a dataset, add an asterisk to the “flag” column of the data_formatting_table, git-add-commit-push the data_source_table, and **add an issue to GitHub**._

### Set-up

1. **Git pull!** Before you begin to work on a dataset, make sure to do a _git pull_ to ensure that you’re working on the most up-to-date version of the core-transient folder. Taking a few seconds to do this may end up saving you minutes in trying to figure out how to deal with git conflicts if they arise.

2. Scratch paper! I suggest always working with a piece of scratch paper to keep track of various issues with the dataset you’re working with. 

3. Open up the _data_formatting_template.R_ (located in _/~core-transient/scripts/R-scripts/data_cleaning_scripts_) script in RStudio.Following and modifying this script will also ensure that all of our scripts are in similar format and are thus easier to follow.Save your script with the naming convention dwork_[datasetID]_[your initials].R and **git add-commit-push**.

4. If this is your first time working on data formatting, I strongly recommend creating an **RStudio Project** to improve workflow. You can do so in R Studio by going to File -> New Project ... -> Existing Directory and adding the location of the core-transient folder on your computer. Once your project is created, it will automatically use the core-transient folder as your working directory (among **many** other benefits).

5. **Load libraries**: In the data formatting process, we tend to rely heavily on certain packages. These include: stringr, plyr, and dplyr. If you do not currently have these packages, be sure to install them.

6. ** Source functions**: To make data formatting and preparation simpler, we have created a set of functions for repetitive and often time-consuming tasks. By running the line of code below, these functions will be automatically loaded into your R environment:

```
source('scripts/R-scripts/core-transient_functions.R')

```

7. Load raw data file and the data formatting table. For example, to read dataset 92, you would run the following lines of script:

```
ds = 92

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

```

### DATA FORMATTING

#### Explore the data

1. When you first load a dataset into R, take some time to explore how the dataset is structured. Common R commands that should be used whenever you start formatting a new dataset include: 

  - `names(example_df)`: Used to observer the field names of the data frame. This is great first look at how you can modify the fields of a data frame to fit the core-transient format.
  - `dim(example_df)`: Used to observe the number of rows and columns of the data frame. To observe just the number of rows, use `nrow(example_df)`
  - `str(example_df)`: Used to observe the structure of the data frame, including how each of the fields are formatted and the fields. Some fields may require to be changed from one format to another. This can be done using: as.character(example_field), as.numeric(example_field), and factor(example_field). _Warning! If you are changing a factor to numeric and want to maintain the field values, you need to use: as.numeric(as.character(example_field))_.
  - `head(example_df)`: Used to observe the first few rows of the data frame. Note that it may sometimes be necessary to observe more rows than the default. You can specify this -- if, for example, you’d like to view the first 10 rows, type `head(example_df, 10)`. You can also look at the last few rows of data using tail(example_df) and modify the number of rows shown as above.
  - `summary(example_df)`: Provide summary data of the data frame. This can be especially useful, for example, to find out if there are zeros in the count data that need to be removed.
  - `class(example_df$site)`: Determine the type of data in a field.

2. Remove unnecessary columns. Many datasets contain columns that we will not use and those columns can be removed. As an example, if you wanted to remove columns 1, 3, 5, 6,  7, 8 from dataset, you could write:

```
dataset1 = dataset[,-c(1,3,5:8)]
```
  
  _Note that the dataset was renamed in the process! This is important, because if you made a mistake along the way, you can very simply revert to the dataset in the previous step. Do this any time you make a modification to the dataset._

3. At this point you may want to rename some of the columns to the naming conventions used for core-transient formatted datasets (datasetID, site, date, species, count). As an example, if you wanted to change the name of the third column of dataset1 to "site" you would write:

```
names(dataset1)[3] = 'site'
```

4. Once you are done with the exploration of the larger dataset and (potentially) removing columns or changing column names, save your script and git-add-commit-push and describe any changes you have made in the commit message.


#### EXPLORE AND FORMAT SITE DATA

At this point, you will need to decide what qualifies as a site for a given study. To do so, you may need to explore the metadata of associated with the study. Study information can be obtained by exploring the study links in the data source table (core-transient/data_source_table.csv). Several datasets have direct links to study metadata.

1. Take a look at the site data (and metadata, if available):

```
head(dataset1$site)
tail(dataset1$site)
```

2. We will need to modify the data formatting table to provide how the sites are coded. This is known as the "Raw_siteUnit" For example, sites for dataset 84 are coded as region, location, latitude, and longitude. We will record the Raw_siteUnit by running the following lines of code:

```
dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'region_location_lat_long') 
```

  a. If the data formatting includes latitudes and longitudes (such as the above example, dataset 84), or are definied by latitudes and longitudes, we need to provide that information on the data formatting table:

```
dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites',   # Fill value in below
  
                                 'Y') 
```
_Note: if the site information is not coded as latitude and longitude, type "N"._

  b. Sites may be nested, either becuase they are latitudes and longitudes that can be evaluated with differing levels of precision (e.g., the latitude 36.374 can be evaluated at a precision of 36.37, 36.3, or 36) or because there are spatial subsamples located within larger samples (e.g., quadrats can be located within larger plots). This information needs to be included in the data formatting table:
  
```
dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'Y') 
```
_Note: if the site information is not nested, type "N"._

3. Out next goal, if sites are nested, is to create a single site field, with site identifiers separated by a "_".

  a. If sites are nested, and sites not defined by latitude and longitude, you will need to determine with the metadata what  constitutes a site and concatenate the field. Importantly, any concatenation must run from the largest to smallest grain site definitions. For example, consider a site defined by "plot" and "quadrat" fields (where quadrats are placed within larger plots):
		
		```
		example_df$site = paste(example_df$plot, example_df$ quadrat)
		```

  b. If sites are coded as lats and longs, concatenate the fields as such:

	```
	example_df$site = paste(example_df$lat, example_df$long, sep = '_')
	```
  _Note that we use an underscore to separate between components of the site field. This will be the case with all site data and using this format consistently is necessary to simplifying field modification in later steps._
	
  c. Lats and Longs and other site data may be embedded in larger field. If this is the case, you may need to use the 	substring function in Hadley Wickham's stringr package to extract the necessary characters.
	
	To extract "hello" in "hello world" (the first five characters), you would use:
  
```
require(stringr)			
str_sub('hello_world', 1, 5)
```

    To extract "world" in "hello world" (the last five characters), you would use:	
  
```
str_sub('hello_world', -5)
```
    
4. git-add-commit-push your script, describing any modification to the site field, if necessary.

    
#### EXPLORE AND FORMAT SPECIES DATA

Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.

1. Take a look at how species are in the dataset (_Note: rename the column if necessary_):

	```
	head(example_df)
		
	sp = example_df$species
		
	class(sp)
		
	length(unique(sp))
	```
2. If species are formatted as characters, it may be easier to format them as factors:

	```
	sp = factor(example_df$species
	```
		
3. Species may be listed in multiple columns, if this is the case, concatenate the columns. For example, if species data are provided in separate genus and species columns, you would use:

	```
	sp = paste(example_df$genus, example_df$species, sep = '')
	```

4. Oftentimes, there is an irregular use of lower and uppercase values. Because R is case sensitive, these would actually be coded as separate species. To change the case of the species column use either of following:

	```
	tolower('Hello World')
		
	toupper('Hello World')
	```
		
5. Explore the metadata to determine how species are coded. This may give some clue of listed species that are not valid. Look at the levels of the species themselves:

	```
	levels(sp)
	```
	
6. Remove bad species (for example, "bare_ground" and "unidentified") by making a vector of bad species names and then subsetting the data to just the valid species):
		
	```
	bad_sp = c('bare_ground','unidentified')
		
	example_df1 = example_df[!example_df$species %in% bad_sp,]
	```
		
7. Explore the dataset to determine how removing these species affected your data frame:
		
	```
	head(example_df1)
		
	summary(example_df1)
		
	nrow(example_df1)
		
	```
		
8. git-add-commit-push your script, describing the removal of species, if necessary.

#### EXPLORE AND FORMAT TIME DATA

Here our goal is simply to format the sampling date. 

1. If dates are provided in multiple columns, it may be necessary to concatenate the columns. 
		
	```
	date = paste(example_df$month, example_df$day, example_df$year, sep '/')
	```
	
2. If the date column is provided only as years, leave date as simply a numeric year.
3. If the date column is in the format of "mm/dd/yyyy", or something similar, convert it to a date object. Check and to make sure that it is properly formatted after the date object is created (will be a POSIX-class object):
	
	```
	date = strptime(date, '%m/%d/%Y')
		
	class(date)
	```
		
4. git-add-commit-push describing any modifications made to the date field.
		
#### EXPLORE AND FORMAT COUNT DATA

Here, our goal is to check what type of count data are provided and remove 0 and NA counts.

1. If counts are "true" counts, enter "count" in the data source table "count_type" field. If counts are actually proportional cover data, enter "cover" in the "count_type" field. If counts are actually density data, enter "density" in the "count_type" field. git-add-commit-push data_source_table.csv.
2. Remove NA's and subset to counts that are greater than 0. After taking the following steps below, be sure to explore the data frame to ensure that the data are formatted correctly:
	
	```
	summary(example_df$count)
		
	example_df = na.omit(example_df)
		
	example_df = subset(example_df, count > 0)
	```
		
3. git-add-commit-push your script, describing any changes you made to the count field.

## MAKE AND WRITE THE FINAL FORMATTED DATASET

1. First, add a datasetID field (for this example, let's say this is dataset 33):
		
	```
	example_df$datasetID = 33
	```
		
2. If date is a POSIX object, convert it to a factor:
	
	```
	example_df$date = factor(as.character(example_df1$date))
	```
	
3. Now use Hadley Wickham's "plyr" package to summarize counts by site, date, and species:
		
	```
	require(plyr)
		
	example_df = ddply(example_df, .(datasetID, site, date, species),
		summarize, count = max(count))
	```
	
4. Explore the dataframe to be sure that everything worked:
		
	```
	dim(example_df)
		
	head(example_df)
		
	summary(example_df)
	```
		
5. Unless date is a numeric vector of years, convert date back into a POSIX object and, if everything looks good, reassign the column:
	
	```
	date = as.Date(example_df$date, '%Y-%m-%d')
		
	class(date)
		
	head(date)
		
	example_df$date = date
	```
	
6. Take a final look and then git-add-push decribing any modifications to the data!
		
	```
	head(example_df)
		
	summary(example_df)
	```
		
7. Write to file (for this example, we'll say this is dataset 33):
		
	```
	write.csv(example_df, 'data/formatted_datasets/dataset_33.csv, row.names = F)
	```
	
8. git-add-commit-push the formatted dataset in the data file, then git-add-commit-push the updated data submodule using the following steps (in git bash, example is for dataset 33):

	```
	cd data
	git add formatted_datasets/dataset_33.csv
	git commit -m "added formatted dataset"
	git push origin master
	cd ..
	git add data
	git commit -m "updated submodule with formatted dataset 33"
	git push origin master
	```
		
## R CODE CHEATSHEET

####  Removing records:

Remove NA's (method 1):
`na.omit(df)`

Remove NA's (method 2):
`df[!is.na(df$species),]`

Remove bad record (method 1):
`df[df!='bad_record',]`

Remove bad record (method 2):
`subset(df, species!= 'bad_record')`

Remove multiple bad records:
`df[!df$species %in% c('bad1', 'bad2', 'bad3'),]`

#### Round numeric vector (x) to a given accuracy:

`round_any(x, accuracy, f = round)`

#### Working with character strings:

Split a column:
`colsplit(string, pattern, names)`

Extract a substring (stringr):
`str_sub(string, start = 1L, end = -1L)`

Concatenate:
`paste('w', 'o', 'r', 'l', 'd', sep = '')`

#### Conversions:

Convert to a character vector:
`as.character(example_df$site)`

Convert to a factor:
`factor(example_df$site)`

Convert factor to numeric:
`as.numeric(as.character(example_df$DatasetID))`

Create data object:
`strptime(df $record_date, '%m/ %d/ %y')`

Extract year from date object:
`as.numeric(format(date, '%Y'))`

#### Summary data:

Number of rows:
`nrow(df)`

Length of vector:
`length(df$site)`

Number of unique elements:
`length(unique(df$site))`

Minimum value:
`min(df$year)`

Maximum value:
`max(df$year)`

Data summary tables:

Tabulate:
`table(df$site)`

Data frame of table (base):
`data.frame(table(df$site))`

Data frame of table (plyr):
`ddply(df, .(site), summarize, 'nrow')`

Summarizing with multiple factors:
`ddply(df, .(site, year, species), summarize, count = sum(count))`
