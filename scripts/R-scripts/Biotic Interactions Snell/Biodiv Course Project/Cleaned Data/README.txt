This document describes data files for 'Data from: Opposing mechanisms drive richness patterns of core and transient bird species' archived in the Dryad Data Repository doi:. 
Bird species richness data was derived from the North American Breeding Bird Survey (BBS) downloaded on January 27, 2011 using the EcoData Retriver (2011). Most environmental data was derived from WorldClim (Hijmans 2005).

This data contains six files:

env_data.csv
	Environmental data associated with 435 BBS routes.

site_sp_occupancy_matrix.csv
	A matrix of the proportion of years for which each bird species was observed on each route.

core-transient_rich_b=0.5.csv
	Species richness of core and transient bird species on 435 BBS routes when a threshold of 50% occupancy is used to define core and transient species groups.

core-transient_rich_b=0.66.csv
	Species richness of core and transient bird species on 435 BBS routes when thresholds of 33.34% and 66.66% occupancy are used to define core and transient species groups.

core-transient_rich_b=0.66_1996-2010.csv
	Species richness of core and transient bird species on 435 BBS routes when thresholds of 33.34% and 66.66% occupancy during the time period of 1996-2010 are used to define core and transient species groups.

core-transient_rich_b=0.75.csv
	Species richness of core and transient bird species on 435 BBS routes when thresholds of 25% and 75% occupancy are used to define core and transient species groups.

The contents and column headings of each data file are described below:
-----------------------------------------
env_data.csv
	stateroute: a unique identifier of the BBS route, equal to 1000*State number + Route number
	Lati: Latitude of route starting coordinates
	Longi: Longitude of route starting coordinates
	regR: Regional species richness, calculated by counting the number of species range maps overlaying each route's starting coordinates. See White and Hurlbert (2010). 
	
All other columns are named using the convention variable.mean or variable.var. '.mean' indicates that the column measures the mean of all pixels in the environmental data layer that intersect each route's geographic path (See USGS 2012). '.var' indicates that the column measures th variance of all pixels within a 40km radius of each route's starting coordinates.

Environmental variable abbreviations are:
	mat		Mean annual temperature, BIO1 - WorldClim
	mdr		Mean diurnal range, BIO2 - WorldClim
	tseas		Temperature seasonality, BIO4 - WorldClim
	tmax		Maximum temperature of the warmest month, BIO5 - WorldClim
	tmin		Minimum temperature of the coldest month, BIO6 - WorldClim
	tar		Temperature annual range, BIO7 - WorldClim
	twarmq		Mean temperature of the warmest quarter, BIO10 - WorldClim
	tcoldq		Mean temperature of the coldest quarter, BIO11 - WorldClim
	ap		Annual precipitation, BIO12 - WorldClim
	pmax		Precipitation in the wettest month, BIO13 - WorldClim
	pmin		Precipitation in the driest month, BIO14 - WorldClim
	pseas		Precipitation seasonality, BIO15 - WorldClim
	pwetq		Precipitation in the wettest quarter, BIO16 - WorldClim
	pdryq		Precipitation in the driest quarter, BIO17 - WorldClim
	sum.NDVI	Mean NDVI, May - August, 2001-2010, derived from product MOD13A3 - LPDAAC (NASA)
	sum.EVI		Mean EVI, May - August, 2001-2010, derived from product MOD13A3 - LPDAAC (NASA)
	elev		Altitude, WorldClim 

NASA LPDAAC: https://lpdaac.usgs.gov/, used 1km resolution grids
WorldClim: http://worldclim.org, used 30 arc-second resolution grids

------------------------------------------
site_sp_occupancy_matrix.csv

This is a matrix whose entries are the proportion of time that each species was observed at each site over all surveys in the BBS. Rows are sites and columns are bird species' codes from the American Ornithologists' Union and are identical to those used in the BBS.


------------------------------------------
core-transient_rich_b=0.5.csv
core-transient_rich_b=0.66.csv
core-transient_rich_b=0.66_1996-2010.csv
core-transient_rich_b=0.75.csv	

These files follow the same column-naming conventions.
	t: Width of time-window (in years) used to calculate richness. Richness was calculated and averaged across all possible time windows of width t within the time period 1996-2010.
	stateroute: a unique identifier of the BBS route, equal to 1000*State number + Route number
	R: Total number of species
	Rcore: Core richness, number of species classified as 'core'.
	Rocca: Transient richness, number of species classified as 'transient'.
	R.aocca: Transient richness when species that are always classified as transient across all routes are removed from the analysis.
	R.ncore: Transient richness when species that are never classified as core on any route are removed from the analysis.	



References

EcoData Retriever. 2011. Ecological Data Wiki. http://ecologicaldata.org/ecodata-retriever.

Hijmans, R. J., S. E. Cameron, J. L. Parra, P. G. Jones, and A. Jarvis. 2005. Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology 25:1965–1978.

USGS Patuxent Wildlife Research Center. The North American Breeding Bird Survey. Retrieved January 27, 2011, from http://www.pwrc.usgs.gov/bbs/.

USGS Patuxent Wildlife Research Center. 2012. The North American Breeding Bird Survey, Route Geographic Information Summaries 1966 - 2003. Version 2004.1. USGS Patuxent Wildlife Research Center, Laurel, MD, USA http://www.pwrc.usgs.gov/bbs/geographic_information/geographic_information_products.htm

White, E. P., and A. H. Hurlbert. 2010. The combined influence of the local environment and regional enrichment on bird species richness. The American Naturalist 175:E35–E43. doi: 10.1086/649578.












