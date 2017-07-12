# Verifying NDVI-richness correlation in Breeding Bird Survey data

# White's lab group has gotten r's of ~0.7, but we only seem to get ~0.5.

# BBS data is same, but one potential source of difference is the scale 
# at which NDVI data is obtained. Here, I believe these are means over
# 40 km radius circle as opposed to White extracting the 1 km pixel.

# Should confirm

bbs = read.csv('z:/lab/databases/bbs/2017/bbs_counts_20170712.csv', header=T)

# Or from rdataretriever:
#  library(rdataretriever)
#  bbsdata = rdataretriever::fetch("breed-bird-survey")
#  bbs = bbsdata$counts
  
good_rtes = bbs %>%
  filter(year > 1999, year < 2015) %>%
  select(year, stateroute) %>%
  unique() %>%
  count(stateroute) %>%
  filter(n == 15) # have to stay at 15 to keep # of years consistent

bbs_sub1 = bbs %>%
  filter(Year > 1999, Year < 2015) %>%
  filter(stateroute %in% good_rtes$stateroute) %>%
  filter(Year > 1999, Year < 2015) %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010) %>%
  select(stateroute, Year, aou, speciestotal)

# Number of unique species observed over the 15-year window by stateroute
rich = bbs_sub1 %>%
  distinct(stateroute, aou) %>%
  count(stateroute)


gimms_ndvi = read.csv("output/tabular_data/gimms_ndvi_bbs_data.csv", header = TRUE)
gimms_agg = gimms_ndvi %>% filter(month == c("may", "jun", "jul")) %>% 
  group_by(site_id)  %>%  summarise(ndvi=mean(ndvi))





# Richness based on 15 year window, 1009 sites with complete sampling during window
ndvirich = rich %>%
  left_join(gimms_agg, by = c('stateroute' = 'site_id'))

cor(ndvirich$ndvi, ndvirich$n, use='pairwise.complete.obs')
#0.502


# Richness of 1009 sites from above window, but only from 2014
ndvirich14 = bbs_sub1 %>%
  filter(Year==2014) %>%
  count(stateroute) %>% 
  left_join(gimms_agg, by = c('stateroute' = 'site_id'))

cor(ndvirich14$ndvi, ndvirich14$n, use='pairwise.complete.obs')
#0.540


# Richness from all sites in 2014 (3171 routes)
allndvirich14 = bbs %>%
  filter(Year==2014) %>%
  filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010) %>%
  count(stateroute) %>% 
  left_join(gimms_agg, by = c('stateroute' = 'site_id'))

cor(allndvirich14$ndvi, allndvirich14$n, use='pairwise.complete.obs')
#0.528