# creating a propOcc plot for each dataset
library(gtools)
library(maps)
library(dplyr)
library(ggplot2)
# setwd("C:/git/core-transient")
# beta = matrix(NA, nrow = length(uniq2), ncol = 19)

pdf('propOcc.pdf', height = 8, width = 10)
par(mfrow = c(3, 4))

source('scripts/R-scripts/core-transient_functions.R')

path = "data/propOcc_datasets/"

out.file<-"output/tabular_data/propOcc.pdf"

file.names <- dir(path, pattern = "*.csv")

file.names <- list.files(path, pattern="*.csv")
file.names = mixedsort(file.names)
#myfiles = lapply(file.names, read.delim)
setwd("C:/git/core-transient/data/propOcc_datasets/")
for(i in 1:length(file.names)){
  dataid <- read.csv(file=file.names[i], header=TRUE, sep=",", stringsAsFactors=FALSE) 
  hist(dataid$propOcc, xlab = "Occupancy", ylab = "Frequency", main = file.names[i])
  # text(x =tempR2.pos, y = .1, paste("R2 = ", round(beta[i,4], 2)), col = "red")
}
#write.table(out.file, file = "cand_Brazil.txt",sep=";", 
 #           row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")
  


dev.off()    #closes plotting device, screen or connxn to a file

##### plots for each spp temporal occupancy ######
library(ggplot2)
library(ggmap)
library(maps)
library(dplyr)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




# setwd("C:/git/Biotic-Interactions")
bbs_occ = read.csv("data/2001_2015_bbs_occupancy.csv", header = TRUE)
bbs_w_aou = bbs_occ %>% filter(aou > 2880) %>%
  filter(aou < 3650 | aou > 3810) %>%
  filter(aou < 3900 | aou > 3910) %>%
  filter(aou < 4160 | aou > 4210) %>%
  filter(aou != 7010)

latlongs = read.csv("data/latlong_rtes.csv", header = TRUE)
AOU = read.csv("data/Bird_Taxonomy.csv", header = TRUE) # taxonomy data

bi_all_exp_pres = read.csv("data/all_expected_pres.csv", header = TRUE)
bi_all_exp_pres = bi_all_exp_pres[,c("Species","stateroute","FocalOcc")]

plotdata = merge(bi_all_exp_pres, latlongs, by = "stateroute") 
plotdata_AOU = merge(plotdata, AOU[,c("AOU_OUT", "PRIMARY_COM_NAME")], by.x = "Species", by.y = "AOU_OUT")
plotdata_all = plotdata_AOU %>%
  mutate(category=cut(FocalOcc, breaks=c(0, 0.34, 0.67, 1), labels=c("transient","intermediate","core")))

subfocalspecies = unique(plotdata_all$Species)
# Making pdf of ranges for each focal spp
colscale = c("red", "gold","dark green")

states <- ggplot2::map_data("state") 

pdf('C:/Git/core-transient/ind_spp_occ_maps.pdf', height = 8, width = 10)
layout = matrix(c(3,3,3,3), nrow=2, byrow=TRUE)
  #matrix(seq(1, 3 * ceiling(439/3)),
              #  ncol = 3, nrow = ceiling(439/3))
plot_list = list()
south_plotsub = c()

for(sp in subfocalspecies){ 
  print(sp)
  plotsub = plotdata_all[plotdata_all$Species == sp,]
  plot_list[[sp]]  = ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
    coord_fixed(1.3) +
    guides(fill=FALSE) + theme_classic() + 
    geom_point(data = plotsub, mapping = aes(x = longitude, y = latitude, col = category),  pch = 20, size = 5)+ scale_color_manual(labels = c("Transient","Intermediate", "core"),values = colscale) + xlab(plotsub$PRIMARY_COM_NAME)
  print (plot_list[[sp]])
  south_sub = plotsub[which(plotsub$latitude <= quantile(plotsub$latitude, 0.25)),]
  south_plotsub = rbind(south_plotsub, south_sub)
}
dev.off()
#ggsave("ind_spp_occ_maps.pdf", height = 8, width = 10)

south_transients = south_plotsub %>%
  group_by(Species, PRIMARY_COM_NAME) %>%
  tally(category == "transient") 
colnames(south_transients) = c("aou","common name", "trans") 

south_nontransients = south_plotsub %>%
  group_by(Species) %>%
  tally(category != "transient")
colnames(south_nontransients) = c("aou","nons") 
  
prop_trans = merge(south_nontransients, south_transients, by = "aou")  
prop_trans$sum = prop_trans$nons + prop_trans$trans
prop_trans$ratio = prop_trans$trans/prop_trans$sum
  
count2 = dplyr::filter(prop_trans,ratio >= 0.49)
envoutput = read.csv("data/envoutput.csv", header= TRUE)

bi_prop_trans = merge(prop_trans, envoutput, by.x = "aou", by.y = "FocalAOU")

whats_left = filter(bi_prop_trans, ratio >= 0.49 & COMP >= 0.49)


library(maps)
library(rgdal)
library(shapefiles)
library(maptools)
library(raster)
library(rgeos)
library(gtools)
library(sp)
library(tidyr)
library(dplyr)

# setwd("C:/git/Biotic-Interactions")
# read in temporal occupancy data from BI occ script
temp_occ = read.csv("data/bbs_sub1.csv", header=TRUE)
temp_occ$Aou[temp_occ$Aou == 4810] = 4812
# read in lat long data
bbs_routes = read.csv("data/latlong_rtes.csv",header =TRUE)
# read in bird range shps
shapefile_path = 'Z:/GIS/birds/All/All/'

all_spp_list = list.files(shapefile_path)



# read in new_spec_weights file created in data cleaning code
new_spec_weights=read.csv("data/new_spec_weights.csv", header=TRUE)
new_spec_weights$focalAOU = as.numeric(new_spec_weights$focalAOU)
new_spec_weights$compAOU = as.numeric(new_spec_weights$CompAOU)
pigeon = new_spec_weights[c(6),]

# for loop to select a genus_spp from pairwise table, read in shp, subset to permanent habitat, plot focal distribution
filesoutput = c()
# dropping non-intersecting polygons
focal_spp = unique(new_spec_weights$focalcat)
new_spec_weights = new_spec_weights[-c(6),]


intl_proj = CRS("+proj=longlat +datum=WGS84")
sp_proj = CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km")

######## Calculating centroids for each species - using whole range #####
pdf('C:/Git/core-transient/buffer_maps.pdf', height = 8, width = 10)
layout = matrix(c(3,3,3,3), nrow=2, byrow=TRUE)
centroid = c()
for (sp in subfocalspecies){
  print(sp)
  plotsub = plotdata_all[plotdata_all$Species == sp,]
  coordinates(plotsub) = c("longitude", "latitude")
  proj4string(plotsub) <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
  plotsub = spTransform(plotsub, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km"))
  t1 = all_spp_list[grep(sp, all_spp_list)]
  t2 = t1[grep('.shp', t1)]
  t3 = strsplit(t2, ".shp")
  
  test.poly <- readShapePoly(paste(shapefile_path, t3, sep = "")) # reads in species-specific shapefile
  proj4string(test.poly) <- intl_proj
  colors = c("blue", "yellow", "green", "red", "purple")
  # subset to just permanent or breeding residents
  sporigin = test.poly[test.poly@data$SEASONAL == 1|test.poly@data$SEASONAL == 2|test.poly@data$SEASONAL ==5,]
  sporigin = spTransform(sporigin, CRS("+proj=laea +lat_0=40 +lon_0=-100 +units=km"))
  #plot(sporigin, col = colors, border = NA)
  srange = gBuffer(sporigin, byid=FALSE, id=NULL, width= -100)
  sbuffer = gDifference(sporigin, srange, byid = FALSE, id=NULL) 
  trueCentroid = gCentroid(sporigin)
  coord = coordinates(spTransform(trueCentroid, CRS("+proj=longlat +datum=WGS84")))
  sporigin = tidy(sporigin)
  
  ggplot(data = sporigin) + 
    geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill = "white") +
    coord_fixed(1.3) +
    guides(fill=FALSE) + theme_classic() + 
    geom_point(data = as.data.frame(coordinates(plotsub)), mapping = aes(x = longitude, y = latitude),  pch = 20, size = 5)+ scale_color_manual(labels = c("Transient","Intermediate", "core"),values = colscale) + xlab(plotsub$PRIMARY_COM_NAME)
  centroid = rbind(centroid, c(sp, coord))
}
dev.off()


centroid = data.frame(centroid)
names(centroid) = c("Species", "FocalAOU", "Long", "Lat")
centroid$Lat = as.numeric(paste(centroid$Lat))
centroid$Long = as.numeric(paste(centroid$Long))
write.csv(centroid,"data/centroid.csv",row.names=FALSE)



