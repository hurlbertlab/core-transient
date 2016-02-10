#Biotic Interactions script

#reading in dataset 
occupancy = read.csv('scripts/R-scripts/data_cleaning_scripts/Biotic Interactions/Master_RO_Correlates_20110610.csv', header = T)
#subsetting species whose occupancies were between 0.3 and 0.6 over a 10 eyar period
subsetocc = occupancy[occupancy$X10yr.Prop > .3 & occupancy$X10yr.Prop < .7,]
#compare green-tailed towhee to spotted towhee
towhees = subsetocc[subsetocc$CommonName == "Spotted Towhee"| subsetocc$CommonName == "Green-tailed Towhee",]

#read in BBS data
bbs = read.csv('data/raw_datasets/dataset_1.csv', header = T)
spotted = bbs[bbs$Aou == 5880,] # subsetting spotted towhees

#merge occupancy with bbs for spotted towhee
t1 = merge(spotted, occupancy, by.x = "Aou", by.y = "AOU")


#BBS abundances of just spotted towhee
#merge occupancy data of species by stateroute
#occupancy on GT, abundance on spotteed
#LT averge for GT towhee for 10 year


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
