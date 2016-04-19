#NOTES
#flycathcers, green tailed towhee
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
#BBS abundances of just spotted towhee
#merge occupancy data of species by stateroute
#occupancy on GT, abundance on spotteed
#LT averge for GT towhee for 10 year



##### modelling working out #####

# Env effects summed
# env_eff = lm(GT_occ ~ eucdist, data = env_occu_matrix)
# Env effects summed and interaction
# both = lm(GT_occ ~  Spotted_abun + eucdist, data = env_occu_matrix)

# summaries using zscore of env variables
#f2 = lm(coyle_o.X5900 ~  SpeciesTotal+abs(zTemp)+abs(zElev)+abs(zPrecip)+abs(zEVI)+abs(euc.dist.spp),
#        data = env_occu_matrix)
#summary(f2)  

# Polynomial Fit for spp
#q2 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2), data = env_occu_matrix)
#summary(q2) 
#q3 <- lm(coyle_o.X5900 ~ SpeciesTotal + I(SpeciesTotal^2) + I(SpeciesTotal^3), data = env_occu_matrix)
#summary(q2) 

#q2_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2), data = env_occu_matrix)
#summary(q2_env)

#q3_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
#               zElev + I(zElev^2) * zEVI + I(zEVI^2), data = env_occu_matrix)
#summary(q3_env)

# q4_env <- lm(coyle_o.X5900 ~ SpeciesTotal * zPrecip + I(zPrecip^2) * zTemp + I(zTemp^2) *
#              zEVI + I(zEVI^2), data = env_occu_matrix) #paring down model decreases significance
# summary(q4_env)