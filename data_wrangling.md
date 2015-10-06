# Core-Transient Dataset Wrangling Sprint

You'll be helping to enter and/or transform a dataset into a standardized format
that will facilitate its analysis under our core vs transient species framework.
Thanks! Below is a guide describing the organization of the repository and 
the steps you are expected to follow.

These steps assume that you have already cloned the core-transient repository
onto your local machine, and that you have set your working directory in R
to that repository using setwd().

*Step 1:*  
Identify a dataset to work on. For the Sprint, choose from the datasets
listed at this [GoogleDoc](https://docs.google.com/spreadsheets/d/1WtfzSf5oEhFdhZcaCyygpR0K0I6EcjPUzwkBfFnfjtY/edit?usp=sharing).
Write your name under the 'data_wrangler' field so that no one else tries to
work on the same dataset. If this is not part of the Sprint, then you may 
just be working down the list of datasets that haven't yet been incorporated
based on the 'data_formatting_table.csv' file in the main repository.

*Step 2:*  
Check to see whether the raw dataset is already in repository. If so, it will
be in the /data/raw_datasets folder, and will be called dataset_XXX.csv, where
XXX is the datasetID from the Data Formatting Table. If there is no dataset
in this folder, then either a digital form doesn't exist (e.g., the data could
be in a table in a published paper which you will need to enter) or it hasn't 
been downloaded yet. 

*Step 3:*  
Click on the link in the 'data_source' field. If there was do raw data file in
the repository, then you should be able to figure out how to obtain the data
from this link. Regardless, this link should take you either to a published paper
or a data repository that has a description of the dataset including all of
the relevant methods that will be useful in understanding that dataset.

*Step 4:*  
Using RStudio, open the 'data_formatting_template.R' which can be found in
/scripts/R-scripts/data_cleaning_scripts/. Immediately click on Save As and 
rename the file as 'dwork_XXX_III.R' where XXX is the datasetID and III are your
initials.

*Step 5:*  
Fill in the the dataset name, paste in the data_source link from the Data
Formatting Table, and add your name in the top commented section.

*Step 6:*  
Follow instructions in the data_formatting_template file. You will first want to
set your working directory in R using something like:

`setwd('c:/git/core-transient/')`

depending on where on your machine you have cloned the core-transient repository.
You should run each line of code, and read along with the comments so you
understand what each section is doing. Whenever you see a comment like this:

\#--! PROVIDE INFO !--\#

this means you are expected to fill in or modify the subsequent line of code
based on the dataset you are working on. Filling in some of this information
will require a detailed understanding of the sampling methodology (what was
the sampling scale, did they have hierarchical sampling levels like quadrats
within plots, how frequently did they sample, etc). You will be constantly 
referring back to the paper or website that describes the study.

FEEL FREE TO ASK QUESTIONS AT ANY TIME IF SOMETHING IS UNCLEAR. ALMOST ALL
DATASETS HAVE *something* WEIRD OR UNCLEAR ABOUT THEM!

Happy wrangling!