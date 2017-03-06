###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

setwd("C:/git/core-transient")

library(lme4)
library(plyr) # for core-transient functions
library(ggplot2)
library(tidyr)
library(maps)
library(gridExtra)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(merTools)
library(digest)


source('scripts/R-scripts/core-transient_functions.R')

# Maximum occupancy of transient species
# (and hence the minimum occupancy of core species is 1 - threshold)
threshold = 1/3

# Number of replicates for randomization tests
reps = 999

##################################################################

# If running summaries for the first time (or wanting to start
# anew because all formatted datasets have changed) and a
# 'core-transient_summary.csv' file does not exist yet in the 
# output/tabular_data folder, or if you just want to get summary
# stats for one or a few datasets into R, run this section

# Specify here the datasetIDs and then run the code below.
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

# BBS (dataset 1) will be analyzed separately for now.
datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

# Other datasets will need to be excluded depending on the particular analysis:
#  --only datasets with countFormat %in% c('count', 'abundance') can be used
#    for species abundance distribution analysis
#  --only datasets with multiple hierarchical spatial scales can be used
#    for the scale analysis
#  --datasets without propOcc values for individual species can only be used
#    in summaries of proportion transient vs core (e.g. d319, d257)
#    (although I think these should get weeded out based on the first criterion)

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
  print(d)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary.csv', 
          row.names = T)

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

# summ = addNewSummariesFun(threshold, reps, write = TRUE)


#####################lump reptile and ampibian into herptile, get rid of invert if possible - other category?, do a table of communities

# Plotting summary results across datasets for Core-Transient analysis
summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$taxa[summ$taxa == "Arthropod"] <- "Invertebrate"
summ$taxa[summ$taxa == "Reptile"] <- NA
summ$system[summ$system == "Aquatic"] <- "Freshwater"
summ$system = factor(summ$system)
summ = na.omit(summ)
summ1 =  subset(summ, !datasetID %in% c(1, 99, 85, 90, 91, 92, 97, 124)) # excluding BBS to include below-scale route info
summ1.5 = summ1[, c("datasetID","site","system","taxa","propCore", "propTrans", "meanAbundance")]
write.csv(summ1.5, "output/tabular_data/summ1.5.csv", row.names = FALSE)
# read in pre formatted bbs below dataset
bbs_summ2 = read.csv("data/BBS/bbs_below_summ2.csv", header = TRUE)
summ2 = rbind(bbs_summ2,summ1.5)
write.csv(summ2, "output/tabular_data/summ2.csv", row.names = FALSE)
dsets = unique(summ2[, c('datasetID', 'system','taxa')])

taxorder = c('Bird', 'Plant', 'Mammal', 'Fish', 'Invertebrate', 'Benthos', 'Plankton')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ2$system)
sitesByTaxa = table(summ2$taxa)

colors7 = c(colors()[552], # plankton
            rgb(29/255, 106/255, 155/255), #bird
            colors()[144], # invert
            colors()[139], # plant
            colors()[551], #mammal
            colors()[17], #benthos
            colors()[637]) #fish

            

symbols7 = c(16, 18, 167, 15, 17, 1, 3) 

taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)

taxcolors$abbrev = taxcolors$taxa
taxcolors$abbrev = gsub("Benthos", 'Be', taxcolors$abbrev)
taxcolors$abbrev = gsub("Bird", 'Bi', taxcolors$abbrev)
taxcolors$abbrev = gsub("Fish", 'F', taxcolors$abbrev)
taxcolors$abbrev = gsub("Invertebrate", 'I', taxcolors$abbrev)
taxcolors$abbrev = gsub("Mammal", 'M', taxcolors$abbrev)
taxcolors$abbrev = gsub("Plankton", 'Pn', taxcolors$abbrev)
taxcolors$abbrev = gsub("Plant", 'Pt', taxcolors$abbrev)

write.csv(taxcolors, "output/tabular_data/taxcolors.csv", row.names = FALSE)

pdf('output/plots/data_summary_hists.pdf', height = 8, width = 10)
par(mfrow = c(2, 2), mar = c(6,6,1,1), cex = 1.25, oma = c(0,0,0,0), las = 1,
    cex.lab = 1)
b1=barplot(dsetsBySystem, col = c('burlywood','skyblue', 'navy')) 
mtext("# Datasets", 2, cex = 1, las = 0, line = 2.5)
barplot(log10(sitesBySystem), col = c('burlywood','skyblue', 'navy'), cex.names = 1, 
        yaxt = "n", ylim = c(0,4)) 
axis(2, 0:4)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
bar1 = barplot(dsetsByTaxa[taxorder], xaxt = "n", axisnames = F,
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
# text(bar1, par("usr")[3], taxcolors$abbrev, adj = c(1, 1), xpd = TRUE, cex = 1) 

mtext("# Datasets", 2, cex = 1.5, las = 0, line = 2.5)
bar2 = barplot(log10(sitesByTaxa[taxorder]), axes = F, axisnames = F, ylim = c(0,3),
               col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
axis(2, 0:4)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
dev.off()


### boxplot summary fig of all time/richness by taxa
pdf('output/plots/numspp_comm.pdf', height = 8, width = 10)
summ1$taxa <-droplevels(summ1$taxa, exclude = c("","All","Amphibian", "Reptile"))
summ1.col = merge(summ1, taxcolors, by = "taxa")
summ1.col$taxa <- factor(summ1.col$taxa,
                    levels = c('Bird','Plant','Mammal','Fish','Invertebrate','Benthos','Plankton'),ordered = TRUE)
rankedtaxorder = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish')

bar1 = boxplot(summ1.col$spRichTotal~summ1.col$taxa, xaxt = "n",  col = as.character(summ1.col$color[match(taxorder, summ1.col$taxa)]))

mtext(expression(" # Species"), 2, cex = 1.5, las = 0, line = 2.5)
dev.off()

pdf('output/plots/numcomm.pdf', height = 8, width = 10)
summ1$taxa <-droplevels(summ1$taxa, exclude = c("","All","Amphibian", "Reptile"))
summ1.col = merge(summ1, taxcolors, by = "taxa")
summ1.col$taxa <- factor(summ1.col$taxa,
                         levels = c('Bird','Plant','Mammal','Fish','Invertebrate','Benthos','Plankton'),ordered = TRUE)
rankedtaxorder = c('Bird','Plant','Mammal','Fish','Invertebrate','Benthos','Plankton')

bar2 = boxplot(summ1.col$nTime~summ1.col$taxa, xaxt = "n",col = as.character(summ1.col$color[match(taxorder, summ1.col$taxa)]))

mtext(expression(" Years of Study"), 2, cex = 1.5, las = 0, line = 2.5)
dev.off()


#### barplot of mean occ by taxa #####
numCT = read.csv("output/tabular_data/numCT.csv", header=TRUE)

# n calculates number of sites by taxa -nested sites
numCT_taxa = numCT %>%
  dplyr::count(site, taxa) %>%
  group_by(taxa) %>%
  tally(n)
numCT_taxa = data.frame(n)
# calculates number of sites by taxa -raw
sitetally = summ %>%
  dplyr::count(site, taxa) %>%
  group_by(taxa) %>%
  dplyr::tally()
sitetally = data.frame(sitetally)

numCT_box=merge(numCT_taxa, taxcolors, by="taxa")

nrank = summ %>% 
  group_by(taxa) %>%
  dplyr::summarize(mean(mu)) 
nrank = data.frame(nrank)
nrank = arrange(nrank, desc(mean.mu.))

summ_plot = merge(summ, nrank, by = "taxa", all.x=TRUE)

summ$taxa <- factor(summ$taxa,
                       levels = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish'),ordered = TRUE)
rankedtaxorder = c('Bird','Mammal','Plankton','Benthos','Invertebrate','Plant','Fish')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ2$system)
sitesByTaxa = table(summ2$taxa)

colorsrank = c(rgb(29/255, 106/255, 155/255), #bird
               colors()[551],#mammal
               colors()[552], # plankton
               colors()[17], # benthos
               colors()[144], # arth
               colors()[139],# plant
               colors()[637]) #fish


symbols7 = c(16, 18, 167, 15, 17, 1, 3) 
taxcolorsrank = data.frame(taxa = unique(summ$taxa), color = colorsrank, pch = symbols7)

w <- ggplot(summ, aes(factor(taxa), mu))+theme_classic()+
  theme(axis.text.x=element_text(angle=90,size=10,vjust=0.5)) + xlab("Taxa") + ylab("Mean Occupancy\n")
w + geom_boxplot(width=1, position=position_dodge(width=0.6),aes(x=taxa, y=mu), fill = taxcolorsrank$color)+
  scale_fill_manual(labels = taxcolors$taxa, values = taxcolors$color)+theme(axis.ticks=element_blank(),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),axis.title.x=element_text(size=22),axis.title.y=element_text(size=22,angle=90,vjust = 1)) + guides(fill=guide_legend(title=""))+ theme(plot.margin = unit(c(.5,.5,.5,.5),"lines")) + annotate("text", x = nrank$taxa, y = 1.05, label = sitetally$n,size=5,vjust=0.8, color = "black")
ggsave("C:/Git/core-transient/output/plots/meanOcc.pdf", height = 8, width = 12)


####### MODELS ######
latlongs = read.csv("data/latlongs/latlongs.csv", header =TRUE)

# merge multiple lat long file to propOcc to get naming convention correct
latlong_w_sites = merge(latlongs, summ2[,c("datasetID", "site", "propTrans")], by = c("datasetID", "site"), all.x = TRUE) 

#drop BBS and add in below scale
latlong_w_sites = subset(latlong_w_sites, !datasetID == 1)

# reformat non multi grain lat longs
dft = subset(dataformattingtable, countFormat == "count" & format_flag == 1) # only want count data for model
dft = subset(dft, !dataset_ID %in% c(1,247,248,269,289,315))
dft = dft[,c("CentralLatitude", "CentralLongitude","dataset_ID", "taxa")]
names(dft) <- c("Lat","Lon", "datasetID", "taxa")
dft2 = merge(dft, summ2[, c("datasetID","site","propTrans")], by = "datasetID")
  
# combining all lat longs, including scaled up data
all_latlongs.5 = rbind(dft2, latlong_w_sites)

# rbind in new BBS data
bbs_be_lat = read.csv("data/BBS/bbs_be_lat.csv", header = TRUE)
# rbind new bbs data to lat longs
all_latlongs =  rbind(bbs_be_lat, all_latlongs.5)
all_latlongs = na.omit(all_latlongs)

# Makes routes into a spatialPointsDataframe
coordinates(all_latlongs)=c('Lon','Lat')
projection(all_latlongs) = CRS("+proj=longlat +ellps=WGS84")
prj.string <- "+proj=longlat +ellps=WGS84"
# Transforms routes to an equal-area projection - see previously defined prj.string
routes.laea = spTransform(all_latlongs, CRS(prj.string))

##### extracting elevation data ####
# A function that draws a circle of radius r around a point: p (x,y)
RADIUS = 5

make.cir = function(p,r){
  points=c()
  for(i in 1:360){
    theta = i*2*pi/360
    y = p[2] + r*cos(theta)
    x = p[1] + r*sin(theta)
    points = rbind(points,c(x,y))
  }
  points=rbind(points,points[1,])
  circle=Polygon(points,hole=F)
  circle
}

routes.laea@data$dId_site = paste(routes.laea@data$datasetID, routes.laea@data$site, sep = "_")
routes.laea@data$unique = 1:16549


#Draw circles around all routes 
circs = sapply(1:nrow(routes.laea@data), function(x){
  circ =  make.cir(routes.laea@coords[x,],RADIUS)
  circ = Polygons(list(circ),ID=routes.laea$unique[x]) 
}
)

circs.sp = SpatialPolygons(circs, proj4string=CRS(prj.string))

# Check that circle locations look right
plot(circs.sp)

elev <- getData("worldclim", var = "alt", res = 10)
alt_files<-paste('alt_10m_bil', sep='')

elev.point = raster::extract(elev, routes.laea)
elev.mean = raster::extract(elev, circs.sp, fun = mean, na.rm=T)
elev.var = raster::extract(elev, circs.sp, fun = var, na.rm=T)

env_elev = data.frame(unique = routes.laea@data$unique, elev.point = elev.point, elev.mean = elev.mean, elev.var = elev.var)
# write.csv(env_elev, "env_elev.csv", row.names = F)
# env_elev = read.csv("env_elev.csv", header = TRUE)

lat_scale_elev = merge(routes.laea, env_elev, by = c("unique")) # checked to make sure order lined up, d/n seem to be another way to merge since DID keeps getting lost
lat_scale_elev = data.frame(lat_scale_elev)

lat_scale_rich = merge(lat_scale_elev, summ2[,c("datasetID","site", "meanAbundance")], by = c("datasetID", "site"))
#  "spRichTrans", 

# Model
mod1 = lmer(propTrans ~ (1|taxa) * log10(meanAbundance) * log10(elev.var), data=lat_scale_rich) 
summary(mod1)

ggplot(data=lat_scale_rich, aes(elev.var,propTrans)) +geom_point(aes(color = as.factor(lat_scale_rich$taxa)), size = 3) + xlab("Elevation Variance")+ ylab("% Transient")+ theme_classic()

# visualizing model results
mod1test = subset(lat_scale_rich, lat_scale_rich$datasetID == 1)
mod1test$scale =  strsplit(mod1test$site,"-")
mod1test$scaled = sapply(mod1test$scale, "[[", 2) # selects the second element in a list
ggplot(data=mod1test, aes(elev.var,propTrans)) + geom_point(aes(color = as.factor(as.numeric(mod1test$scaled))), size = 3)+ xlab("Elevation Variance")+ ylab("BBS Scaled % Transient")  + theme_classic() 
hist(mod1test$propTrans)

# simple linear model based on data in Fig 2b of % transient ~ taxonomic group, just to have a p-value associated with the statement "The proportion of an assemblage made up of transient species varied strongly across taxonomic group."
transmod = lm(pTrans~taxa, data = CT_long)
summary(transmod)

