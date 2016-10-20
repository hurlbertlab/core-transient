### This script derives occupancy data using ecoretriever to subset raw BBS data



##### ecoretriever to download bbs data and derive occupancy values #####
bbs_eco = ecoretriever::fetch("BBS") # takes forever!!

Years = (bbs_eco$counts$Year)
bbs_eco$counts$Year = as.numeric(bbs_eco$counts$Year)
bbs_eco$counts$stateroute = bbs_eco$counts$statenum*1000 + bbs_eco$counts$Route

# Get subset of stateroutes that have been surveyed every year from 1996-2010
good_rtes = bbs_eco$counts %>% 
  filter(Year >= 1996, Year <= 2010) %>% 
  dplyr::select(Year, stateroute) %>%
  unique() %>%    
  group_by(Year) %>% 
  count(stateroute) %>% 
  #tally(Year) 
  filter(n == 15) #%>% # have to stay at 15 to keep # of years consistent

# Calculate occupancy for all species at subset of stateroutes above
bbs_sub1 = bbs_eco$counts %>% 
  filter(Year > 1995, Year < 2011, stateroute %in% good_rtes$stateroute) %>% 
  dplyr::select(Year, stateroute, Aou) %>%
  #group_by(stateroute, Aou) %>% 
  unique() %>%
  count(Aou, stateroute) 

bbs_sub1$occ = bbs_sub1$n/15 # new occupancy values calculated
write.csv(bbs_sub1, "bbs_sub1.csv", row.names=FALSE)