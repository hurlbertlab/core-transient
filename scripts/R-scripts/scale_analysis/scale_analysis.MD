This .md details the overarcing objectives of the Core-Transient scale analyses, hypotheses, and associated questions. 
It also highlights which pieces of .R code cover specific portions of these analyses. 


2 primary components: 

1) characterize how calculated occupancy patterns in BBS alone vary with scale 

* for just bbs 

* shift in temporal occupancy from transient to core 

	- how does that shift happen and what impacts it? 

* geographic context within a site sits: landscape heterogeneity, ndvi 

	- we already know bbs routes in more hetero vs homog at same scale has more transient spp 

* scale patterns: could be that we expect mean occupancy to increase with higher spatial scales 

* how do these patterns differ between homogenous and heterogenous landscapes as scale increases? 

	- might include diffs in slope and leveling off between the two landscapes when plotted as separate curves

	- as we change scale in a more hetero region or route, how does temporal occupancy vary? 

	- at the scale of a bbs route, of sub-route segments comprised of collapsed stop data, and at the scale above a bbs route 

* can extract temporal occupancy values for every spp 

	- most immediate piece to work on first

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


# Notes on exploring the impacts of spatial scale on temporal occupancy

* Run models comparing discrepancies at different scales. 

* Does scale of sample play a role in explaining discrepancies between sites? 
 
	- Run variation partitioning analysis for each scale. 

	- Compare outcomes. 

