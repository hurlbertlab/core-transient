# Preparing datasets for analysis 

In this project, we evaluate the proportion of core and transient species at a given site using data collected across a wide range of taxa, regions, and environmental systems. A challenge that we face is that ecological data are collected at highly variable spatial, temporal, and biological scales and it is necessary to consistently format the data in preparation for analysis. Here we provide instructions associated with formatting datasets and then creating a proportional occurrence and dataset summary files. This document is divided into three parts. **Section One** provides instructions on how to format a raw dataset to a consistent set of fields at the finest available spatial and temporal grain. **Section Two** provides instructions on how to make a proportional occurrence data frame after summarizing the data at a given spatial or temporal scale. **Section Three** provides an "R Cheatsheet" to the various functions you will likely use in the preparation of data for analysis.

## SECTION ONE: CREATING FORMATTED DATASETS

In the formatting process, we will avoid making any decisions on scale. Instead, we want to ensure that the fields are all consistently of the same structure, that only valid species are listed, and count data are summarized by site, date, and species. 

The overall goal in this step of the process is to format a dataset as follows:

datasetID | site | species | date | count
--------- | ---- | ------- | ---- | -----
01 | d01_Treatment1PlotA | A._schoenobaenus | 1928 | 1
01 | d01_Treatment1PlotA | Buteo_buteo | 1928 | 3
01 | d01_Treatment1PlotB | Corvus_corax | 1929 | 7
01 | d01_Treatment2PlotA | Cuculus_canorus | 1929 | 1
01 | d01_Treatment2PlotB | Gallinula_chloropus | 1928 | 2


Below are the steps that you should take when exploring and formatting datasets. These steps should be followed in the order that they’re presented.  _**IMPORTANT**: throughout this process, if there are any problems that keep you from successfully formatting a dataset, add an asterisk to the “flag” column of the data_source_table, git-add-commit-pull the data_source_table, and **add an issue to GitHub** (assigning the issue to me)._

1. Git pull! Before you begin to work on a dataset, make sure to do a **git pull** to ensure that you’re working on the most up-to-date version of the core-transient folder. Taking a few seconds to do this may end up saving you minutes in trying to figure out how to deal with git conflicts if they arise.

2. Scratch paper! I suggest always working with a piece of scrap paper to keep track of various issues with the dataset you’re working with. 

3. Open up the _data_formatting_template.R_ (located in _/~core-transient/scripts/R-scripts/data_cleaning_scripts_) script in RStudio.Following and modifying this script will also ensure that all of our scripts are in similar format and are thus easier to follow.Save your script with the naming convention dwork_[datasetID]_[your initials].R and **git add-commit-push**.

4. Read in a raw data file, for example: `read.csv('data/raw_datasets/dataset_223.csv`

5. Explore! When you first load a dataset into R, take some time to explore the data. Common R commands that should be used whenever you start formatting a new dataset include: 

  1. `names(example_df)`: Used to observer the field names of the data frame. This is great first look at how you can modify the fields of a data frame to fit the core-transient format.
  2. `dim(example_df)`: Used to observe the number of rows and columns of the data frame. To observe just the number of rows, use `nrow(example_df)`
  3. `str(example_df)`: Used to observe the structure of the data frame, including how each of the fields are formatted and the fields. Some fields may require to be changed from one format to another. This can be done using: as.character(example_field), as.numeric(example_field), and factor(example_field). _Warning! If you are changing a factor to numeric and want to maintain the field values, you need to use: as.numeric(as.character(example_field))_.
  4. `head(example_df)`: Used to observe the first few rows of the data frame. Note that it may sometimes be necessary to observe more rows than the default. You can specify this -- if, for example, you’d like to view the first 10 rows, type `head(example_df, 10)`. You can also look at the last few rows of data using tail(example_df) and modify the number of rows shown as above.
  5. `summary(example_df)`: Provide summary data of the data frame. This can be especially useful, for example, to find out if there are zeros in the count data that need to be removed.
  6. `class(example_df$site)`: Determine the type of data in a field.

6. Remove unnecessary columns. Most datasets contain columns that we will not use and those columns can be removed. As an example, if you wanted to remove columns 1, 3, 5, 6,  7, 8 from example_df, you could write:

	```
	example_df1 = example_df[,-c(1,3,5:8)]
	```
_**Note**: In the above I added a "1" to the example_df name. I consider this best practices -- you can check your work and only overwrite the R object if there were no errors. This will save you from having to rerun portions of the script._

7. Once you are done with the exploration of the larger dataset and (potentially) removing columns, save your script and git-add-commit-push and describe which columns were removed and why.

8. Explore and format **site** data. At this point, you will need to decide what qualifies as a site for a given study. To do so, visit the metadata of a site with the link provided in the metadata field of the data source table (core-transient/data_source_table.csv). 
	1. If sites are coded as lats and longs, concatenate the fields as such:

		```
		example_df$site = paste(example_df$lat, example_df$long, sep = '_')
		```
		
		1. Note that we use "_" to separate between components of the site field. This will be the case with all site data and using this format consistently is necessary to simplifying field modification in later steps.
		2. Lats and Longs may be embedded in larger field. If this is the case, you may need to use the 	substring function in Hadley Wickham's stringr package to extract the necessary characters.
		
			To extract "hello" in "hello world" (the first five characters), you would use:

			```
			require(stringr)
			
			str_sub('hello_world', 1, 5)
			```
			
			To extract "world" in "hello world" (the last five characters), you would use:
			
			```
			str_sub('hello_world', -5)
			```
			
		2. **Important**: If sites are defined by lats and longs, mark "Y" in the "spatial_sites" column of the data source table. If the spatial grain can be varied (for example "rounding" lats and longs to different precisions), then enter "Y" in the "spatial_scale_variable" field of the data source table. git-add-push data_source_table.csv
		
	2. If sites are not coded as lats and longs, use the metadata to determine what consititutes a site. Rename the column as "site", if necessary. For example, if site is the first column, use:
		
		```
		names(example_df)[1] <- 'site'
		```
	
		1. If site information is stored in multiple fields, you will need to determine with the metadata what  constitutes a site and concatenate the field. For example, consider a site defined by "plot" and "quadrat" fields:
		
			```
			example_df$site = paste(example_df$plot, example_df$ quadrat)
			```
		
		2. **Important**: Enter an "N" in the spatial sites column of the data source table. If the component site fields are nested (for example quadrats within plots), enter "Y in the "spatial_scale_variable" field of the data source table. git-add-push data_source_table.csv
		
	
9. Explore and format **species** data: Here, your primary goal is to ensure that all of your species are valid. To do so, you need to look at the list of unique species very carefully. Avoid being too liberal in interpretation, if you notice an entry that MIGHT be a problem, but you can't say with certainty, create an issue on GitHub.
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
		
	7. Explore the dataset to determine how removing these species affected your data frame. If all looks okay, rename:
		
		```
		head(example_df1)
		
		summary(example_df1)
		
		nrow(example_df1)
		
		example_df = example_df1
		```
		
	8. git-add-commit-push your script, describing the removal of species, if necessary.

10. Explore and format the time data. Here we need to extract sampling date. 
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
		
11. Explore and format the count data. Here we need to check what type of count data are provided and remove 0 and NA counts.
	1. If counts are "true" counts, enter "count" in the data source table "count_type" field. If counts are actually proportional cover data, enter "cover" in the "count_type" field. If counts are actually density data, enter "density" in the "count_type" field. git-add-commit-push data_source_table.csv.
	2. Remove NA's and subset to counts that are greater than 0. After taking the following steps below, be sure to explore the data frame to ensure that the data are formatted correctly:
	
		```
		summary(example_df$count)
		
		example_df1 = na.omit(example_df)
		
		example_df1 = subset(example_df1, count > 0)
		```
		
	3. git-add-commit-push your script, describing any changes you made to the count field.
12. You're now ready to make the final formatted dataset!
	1. First, we'll make our "safe" dataset and add a datasetID field (for this example, let's say this is dataset 33):
		
		```
		example_df1 = example_df
		
		example_df1$datasetID = 33
		```
		
	2. If date is a POSIX object, convert it to a factor:
	
		```
		example_df1$date = factor(as.character(example_df1$date))
		```
	3. Now use Hadley Wickham's "plyr" package to summarize counts by site, date, and species:
		
		```
		require(plyr)
		
		example_df2 = ddply(example_df1, .(datasetID, site, date, species),
			summarize, count = max(count))
		```
	
	4. Explore the dataframe to be sure that everything worked:
		
		```
		dim(example_df2)
		
		head(example_df2)
		
		summary(example_df2_
		```
		
	5. Unless date is a numeric vector of years, convert date back into a POSIX object and, if everything looks good, reassign the column:
	
		```
		date = as.Date(example_df2$date, '%Y-%m-%d')
		
		class(date)
		
		head(date)
		
		example_df2 = date
		```
	
	6. Take a final look and then git-add-push decribing any modifications to the data!
		
		```
		head(example_df2)
		
		summary(example_df2)
		```
		
	7. Write to file (for this example, we'll say this is dataset 33):
		
		```
		write.csv(example_df2, 'data/formatted_datasets/dataset_33.csv, row.names = F)
		```
	
	8. git-add-commit-push the formatted dataset in the data file, then git-add-commit-push the updated data submodule using the following steps (in git bash, example is for dataset 33):

		```
		cd data
		git add formatted_datasets/dataset_33.csv
		git commit -m "added formatted dataset"
		git push
		cd ..
		git add data
		git commit -m "updated submodule with formatted dataset 33"
		git push
		```
		
## SECTION TWO: CREATING PROPORTIONAL OCCURRENCE AND DATASET SUMMARY DATA FRAMES
		
We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. We've alreay done the heavy lifting of dataset formatiting and it's now to make some scale decisions and determine the proportional occupancies. To do so, we need to explore the dataset and look through the metadata for clues to the approriate temporal and spatial grain.

### Temporal data:

Because it's often considerably more straightforward, we'll start with the temporal data. Here, our goal is to determine what the sampling interval was for a given site, determine whether the sampling interval meets the needs of our study and convert the date column to a numeric "year" field. Sampling intervals of less than a year are provided as decimal years (e.g., if data were collected monthly, January of 2015 might be coded as 2015.0, whereas December might be coded as 2015.92). Importantly, this numeric value is simply used to differentiate between sampling periods -- the number itself really doesn't matter much.

As an example, we'll look at dataset 223, in which sites were sampled at fall and winter time intervals but reported as the day they were sampled. Again, looking at the metadata is very important to determine the sampling interval. We'll use dataset 223 as an example throughout this section. If you're following along with this we must first make sure to set-up:

```
library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_223.csv")
```

Start by exploring the date field:

```
class(dataset$date)

head(dataset$date)
```

To separate the date into two seasons, we'll extract the month, convert the field to numeric, and then explore the data.

```
month = as.numeric(format(dataset$date, '%m'))

head(month)

summary(month)

unique(month)
```

To make the "season" values (recalling that it is the separate sampling occasions that matter rather than the month per se, we'll use an ifelse statement that separates the sampling occasions:

```
season = ifelse(month < 7, .25, .75)
```

Next, we'll extract year from the date and add the two columns to get a decimal date:

```
year = as.numeric(format(dataset$date, '%Y'))

yearSeason = year + season
```

And change the date in the dataset to our new decimal date format and then rename the column year:

```
dataset1 = dataset

dataset1$date = yearSeason

names(dataset1)[3] = 'year'
```
Finally, we will summarize the dataset to the new temporal grain, summarizing the maximum count for a given site and sampling period. Explore the data to ensure that the values are correct then reassign the name dataset once you're sure:

```
dataset2 = ddply(dataset1, .(datasetID, site, year, species), 
                 summarize, count = max(count))

dim(dataset2)      

head(dataset2)

summary(dataset2)

dataset = dataset2
```





## SECTION THREE: R CODE CHEATSHEET

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
