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

subfocalspecies = unique(bbs_w_aou$aou)
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









