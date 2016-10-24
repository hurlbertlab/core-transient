#Molly F. Jenkins 
#10/24/2016 
#Core-Transient Scale Analysis: Above-route analyses for best sampling practice 

#This script was developed to aid in determining the best # of routes to sample for each grid cell size across BBS data. 
#It is also an active script for developing a "moving window" approach to grid cells. 
#Eventual output should be a table with a "grain" column and a corresponding "sample_size" column
#This table will replace the current "grain_sample" table in the occ_dist_vs_scale.R script. 


#-----------------------------------------------------------------------------------------

####Determining ideal magic number "X" assigned to each grain; creating this file to draw from to use in below
#in lieu of hardcoding grain and sample_n portions prior to loops 

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)

bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)

# bring in bbs routes file 
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


# merge lat longs from routes file to the list of "good" routes
require(dplyr)
temproutes = good_rtes2 %>% 
  left_join(routes, good_rtes2, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)


grain = 10

map_threshold = function(grain, thresh) {
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  
  ct = temproutes %>% count(latbin, longbin)
  
  map('state')
  points(ct$longbin[ct$n >= thresh], ct$latbin[ct$n >= thresh], 
         cex = log10(ct$n[ct$n >= thresh]), pch = 16)
  leg_benchmarks = c(2, max(ct$n)/2, max(ct$n))
  legend("bottomright", legend = c(2, max(ct$n)/2, max(ct$n)), pch = 16,
         pt.cex = log10(leg_benchmarks))
  
}

text(ct$longbin, ct$latbin, ct$n)



hist(ct$n)
median(ct$n)

quantile(ct$n, 0.6)


####What cells at the coarse grid scale are best for sampling across scales?####
