This .md details the overarching objectives of the Core-Transient scale analyses, hypotheses, and associated questions. 
It also highlights which pieces of .R code in the folder are associated with specific objectives of these analyses. Presently, the entirety of the 1st component is in the occ_dist_vs_scale.R file. The unfinished 2nd component is spread between Gartland_code.R and Spring_code_updates.R 


2 primary components: 

1) characterize how calculated occupancy patterns in BBS alone vary with scale 

* shift in temporal occupancy from transient to core 

	- how does that shift happen and what impacts it? 

* geographic context within a site: landscape heterogeneity, ndvi 

	- we already know bbs routes in more heterogenous landscape have more transient species than homogenous landscapes at the same scale

* scale patterns: we expect mean occupancy (incidence of core species designation) to increase with larger spatial scales 

* how do occupancy patterns differ between homogenous and heterogenous landscapes as scale increases? 

	- might include diffs in slope and leveling off between the two landscapes when plotted as separate curves

	- as we change scale in a more heterogenous region or route, how does temporal occupancy vary:

	- i) at the scale of a bbs route
	
	- ii) at scales below a BBS route
	
		- at different scale-associated BBS route segments comprised of collapsed 50 stop data 
		
		- segments: collapse and calc occupancy for every 25, 10, 5 stops (2, 5, 10 segments generated per BBS route) 
		
		- calculate spp occupancy within a given segment 
	
	- iii) and at scales above a BBS route? 
	
		- across clustered routes 
		
		- routes clustered by # of routes that fall within a latitudinal/longitudinal grid cell or "bin" 
		
		- bin size (grid cell size) dictated by grain and extent (X # routes sampled within each grid cell/bin) 
		
		- grain = degree dimensions of a given grid cell/bin (1, 2, 4, 6, 8, 10 - currently experimenting with sizes) 
		
		- calculate spp occupancy across routes within a given grid cell/bin based on fixed # of routes sampled for that grain 

* can extract temporal occupancy values for every spp 

	- plot against averages 

2) Characterize the degree to which Breeding Bird Census (BBC) data validate and agree with BBS occupancy data 

* compare BBS occupancy with BBC observer-defined occupancy categories 

* a) pair BBS and BBC sites by geographic distance 
	
	-compare category agreements and discrepancies 
	
	-run for multiple scales: 
		
	- i) at single-route-site paired scale
		
	- ii) at scales beneath a single BBS route, encompassing different segment lengths w/in a given route 
			
	-because BBC sites are often irregular in size and smaller than a single BBS route, may be closer to a single segment of a route than the entire route itself 
	
* b) pair BBS and BBC sites by distance in environmental space (i.e. axes are environmental variables: NDVI, elevation, precip, etc.) 

	-compare category agreements and discrepancies
	
	-run for multiple scales: 
		
	- i) at single-route-site paired scale
		
	- ii) at scales beneath a single BBS route, encompassing different segment lengths w/in a given route 
			
	-because BBC sites are often irregular in size and smaller than a single BBS route, may be more environmentally similar to a single segment of a route than the entire route itself 

# Roadmap: 
* alt_occ_scale.R -> Script that calculates occupancy for both above and below BBS-route scales. Identifies relationship between occupancy and scale (in both km and in abundance as a proxy for scale). Extraction of coefficients for each stateroute, predicted values for each stateroute and scale. 

* env_analysis.R -> Script that extracts environmental variables of interest (NDVI, Elevation, Precipitation, Temperature) for each stateroute (ideally also scale-specific extractions). Evaluates relationships between individual environmental variables and coefficients from occupancy-scale curves. Do these variables have bearing on variation in the relationship between occupancy and scale? 

### Older analyses for reference: 
* Spring_Code_updates -> Environmental analyses for both BBS and BBC sites. Extraction of environmental data for BBC sites. Pairing of sites by minimum distance in both geographic and environmental space. 

* Gartland_Code -> original prototypical code for Component II for FS 2015 Macroecology project.

* Grid_sampling_justification -> code exploring number of BBS routes available per region across the United States and appropriate cutoff thresholds for aggregating routes in above-route scale calculations. Describes how we arrived at a cutoff of 66 BBS routes aggregated for each focal route as our maximum scale. 

# Notes on exploring the impacts of spatial scale on temporal occupancy

* Run models comparing discrepancies at different scales. 

* Does scale of sample play a role in explaining discrepancies between sites? 
 
	- Run variation partitioning analysis for each scale. 

	- Compare outcomes. 

