# Dataset formatting notes

Here, we will provide information describing the steps taken in formatting a dataset. Copy-and-paste the template section below and enter the described relevant information.

---------------------------------------

## Dataset_template

* **Dataset formatted on, by**: Formatting date, your initials

* **Formatting completed?** Yes or No
  * **If no, what problems did you run into?** Here, describe 

* **Site notes:** Here, provide any information necessary to describe any decisions and changes made to create the site field.
  * **LatLon sites?** Yes or No decribing whether the sites determined using latitude and longitude information
  *  **If LatLon, spatial grain?** Describe the spatial grain used and how this decision was made, and steps taken to create sites. Else, NA
  *  **Are the LatLons of the sites available?** Yes or No.
  *  **Can the data be used to assess the effects of spatial grain** Yes or No. We want to look at the influence of spatial grain on core-transient designation. We may be able to do this if the sites are nested (e.g., quadrats within a plot) or they can be scaled using Lats and Lons.

* **Time notes** Here, provide any information necessary to describe any decisions and changes made to create the year field.
  * **Temporal grain less than or greater than one year?** Yes or No
  * **If < or > one year, temporal grain?** Describe the temporal grain used and how this decision was made, and steps taken to create sites. Else, NA

* **Species notes:** Here, describe decisions that you made in excluding or including species. For example, perhaps you excluded a species because it was listed as "grass" or included a taxa that was identified to family while the majority of the taxa in the study were reported to genus. Likewise, perhaps you included a taxa that was identified by common name when the rest of the taxa were identified by scientific names. Make sure the decisions you made are described thoroughly enough for reproducibility.

* **Count notes:** Here, describe any modifications that were made to the count field. For example, perhaps the "count" column is actually proportional cover or density. As a more complicated example, perhaps the count was provided as the number of individuals within a sampling bottle and the bottles were of different sizes (in which case, you would adjust the by the size of the bottle). There are some instances in which there are no count data available. If this is the case, be sure to enter that information here.

* **Did you update any information in the datasource table? If so, what was updated?** Provide information here.
* **Additional notes:** Provide any additional notes here.

---------------------------------------

---------------------------------------

## Dataset_239

* **Dataset formatted on, by**: 1/28/15, bse

* **Formatting completed?** No
  * **If no, what problems did you run into?** This is pre-formatting. The data are provided in 3 separate csv files. I'm consolidating the data into a single file that can be further formatted as necessary. The script for this is stored as **d239_preFormat.R** in the folder *core-transient/scripts/R-scripts/data_cleaning_scripts*

* **Site notes:** Sites are coded as "SampleID" in table 1. This is a bit unclear, however, as the number of sites differ somewhat from the value presented in the [metadata](http://esapubs.org/archive/ecol/E094/149/metadata.php) (1048 SampleID's rather than 788 as stated in the metadata).
  * **LatLon sites?** Yes or No decribing whether the sites determined using latitude and longitude information
  *  **If LatLon, spatial grain?** Describe the spatial grain used and how this decision was made, and steps taken to create sites. Else, NA
  *  **Are the LatLons of the sites available?** Yes or No.
  *  **Can the data be used to assess the effects of spatial grain** Yes or No. We want to look at the influence of spatial grain on core-transient designation. We may be able to do this if the sites are nested (e.g., quadrats within a plot) or they can be scaled using Lats and Lons.

* **Time notes** Here, provide any information necessary to describe any decisions and changes made to create the year field.
  * **Temporal grain less than or greater than one year?** Yes or No
  * **If < or > one year, temporal grain?** Describe the temporal grain used and how this decision was made, and steps taken to create sites. Else, NA

* **Species notes:** Here, describe decisions that you made in excluding or including species. For example, perhaps you excluded a species because it was listed as "grass" or included a taxa that was identified to family while the majority of the taxa in the study were reported to genus. Likewise, perhaps you included a taxa that was identified by common name when the rest of the taxa were identified by scientific names. Make sure the decisions you made are described thoroughly enough for reproducibility.

* **Count notes:** Here, describe any modifications that were made to the count field. For example, perhaps the "count" column is actually proportional cover or density. As a more complicated example, perhaps the count was provided as the number of individuals within a sampling bottle and the bottles were of different sizes (in which case, you would adjust the by the size of the bottle). There are some instances in which there are no count data available. If this is the case, be sure to enter that information here.

* **Did you update any information in the datasource table? If so, what was updated?** Provide information here.
* **Additional notes:** Provide any additional notes here.

---------------------------------------


