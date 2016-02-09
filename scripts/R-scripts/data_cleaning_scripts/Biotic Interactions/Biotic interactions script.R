#Biotic Interactions script

#reading in dataset 
occupancy = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/Master_RO_Correlates_20110610.csv', header = T)
#subsetting species whose occupancies were between 0.3 and 0.6 over a 10 eyar period
subsetocc = occupancy[occupancy$X10yr.Prop > .3 & occupancy$X10yr.Prop < .7,]
#compare green-tailed towhee to spotted towhee
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

#read in BBS data
bbs = read.csv('data/raw_datasets/dataset_1.csv', header = T)
subsettowhee = bbs[bbs$Aou == 5880|bbs$Aou == 5900,] # just two desired spp of towhee
#select overlapping routes
t1 = table(subsettowhee$stateroute, subsettowhee$Aou == 5880) 

if (t1$subsettowhee.Aou....5880 == TRUE & t1$subsettowhee.Aou....5880 == FALSE) {
  cbind(1)}

#for each site, we want the abundance of competitors for GT towhee


#NOTES
#flycathcers, green tailed tohee
#most likely asymmetric interactions - one sp will be dominant
#clue could be occupancy values themselves - green tailed tohee (spotted tohee)
#look at sparrows too - same sized birds
#MAcArthurs warblers paper- fine-scale partitioning
#get a bird phylogeny -  family specific
#functional diversity - Jes lichen paper 
#add continuous variables later
#chose one closely related species
# there are x sp with an occupancy in this range, for each sp examined a funcitonally
# and phylogenetically closest species and results suggest this is excting
#have one key value
#pick one species or sum across species - competitor is sum of all other of same species
#agreggate by bbs route
#start with one-by-one towhee
#every row is a site, index is abundance of potential competitors
