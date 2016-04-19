# Code from Jes Coyle modified by Allen Hurlbert

# Dryad data file (http://datadryad.org/resource/doi:10.5061/dryad.q82nn)
# 'site_sp_occupancy_matrix.csv' and BBS file 'routes.csv'

occupancy.matrix = read.csv('/Users/terrysnell/Desktop/Cleaned Data/site_sp_occupancy_matrix.csv', row.names = 1)
                             
route.locs = read.csv('/Users/terrysnell/Desktop/routes.csv')
route.locs$stateroute = 1000*route.locs$statenum + route.locs$Route

category.matrix = occupancy.matrix
category.matrix[is.na(occupancy.matrix)] = 'A'  #absent 
category.matrix[occupancy.matrix >= 2/3 & occupancy.matrix <= 1] = 'C' #core
category.matrix[occupancy.matrix <= 1/3] = 'T' #transient
category.matrix[occupancy.matrix > 1/3 & occupancy.matrix < 2/3] = 'M' #moderate

install.packages('gstat')
install.packages('sp')
install.packages('rgdal')
library('sp')
library('gstat')
library('rgdal')
install.packages("raster")

#SARAS EDITS

tmin <- getData("worldclim", var = "tmin", res = 10)
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))
plot(tmin1)
newext <- drawExtent()  # click twice on the map to select the region of interest
tmin1.c <- crop(tmin1, newext)
plot(tmin1.c)
list.ras <- mixedsort(list.files(paste(getwd(), "/wc10/", sep = ""), full.names = T, pattern = ".bil"))
list.ras  # I have just collected a list of the files containing monthly temperature values
tmin.all <- stack(list.ras)
tmin.all <- tmin.all/10
tmin.all.c <- crop(tmin.c, newext)
plot(tmin.all.c)

# Read in stack of layers from all 12 months 1990 (doesn't work for these datasets for some reason)
files1<-paste('/Users/terrysnell/Desktop/prec_10m_bil',1:9,'_bil.bil',sep='')
files2<-paste('Users/terrysnell/Desktop/prec_10m_bil',10:12,'_bil.bil',sep='')
files= c(files1, files2)
tmean<-stack(files)
plot(tmean)

#climate_var - mean(climate_var across the range)) / var(climate_var_across the range)
#mean_temp = read.csv('/Users/terrysnell/Desktop/tmean_10m_bil')
library('raster')
# set the working directory to the path that contains your .bil files
setwd('/Users/terrysnell/Desktop/tmean_10m_bil')
# create a list of .bil files that exist in the wd
files <- list.files(pattern='\\.bil$')
# vars is a vector of bioclim variable numbers
vars <- sort(unique(as.numeric(gsub('^bio([0-9]+)_.*', '\\1', files))))
# for each of vars, create raster object for each tile and merge
# (this is a bit messy, but all I could think of for now...)
# grids will be a list of rasters, each of which is the merged tiles for a BC var.
grids <- sapply(vars, function(x) {
  patt <- paste('bio', x, '_', sep='')
  tiles <- files[grep(patt, files)]
  merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
                                                           sep='"')), ')', sep='')))
})
# give the list elements names
names(grids) <- paste('bio', vars, sep='')
# combine all list elements into a stack
s <- stack(grids)
# quick plot to make sure nothing went drastically wrong
plot(s)
# crop to your study extent
s.crop <- crop(s, WashingtonBoundary)
plot(s.crop)

##################################################################################

#Make a dataframe for plotting
sproute.data = expand.grid(rownames(occupancy.matrix), colnames(occupancy.matrix))
names(sproute.data) = c('stateroute','Aou')
sproute.data$stateroute = as.numeric(as.character(sproute.data$stateroute))
sproute.data$Aou = as.character(sproute.data$Aou)
sproute.data$occupancy = as.vector(as.matrix(occupancy.matrix))
sproute.data$code = factor(as.vector(as.matrix(category.matrix)), levels = c('A','T','M','C'))

#Add route locations and make into spatial data
prj.string = "+proj=laea +lon_0=-100 +lat_0=40 +ellps=sphere"
sproute.data = merge(sproute.data, route.locs, by='stateroute', all.x=T)
sproute.data2 = sproute.data
coordinates(sproute.data) = c('Longi','Lati')
proj4string(sproute.data)<-CRS("+proj=longlat")
sproute.data = spTransform(sproute.data, CRS(prj.string))

#Subset by species
species = colnames(occupancy.matrix)
i=1

spplot(sproute.data[sproute.data$Aou==species[i],],'occupancy',
	names.attr=c(species[i]),
	col.regions=colorRampPalette(c("Blue","Light Blue","Dark Green","Yellow","Red"))(10),
	key.space=list(title="Temporal Ocupancy",cex.title=0.8,space="right",border=F,cex=0.8),
	cuts=seq(0,1,0.1),pretty=T
)

pdf('/Users/terrysnell/Desktop/Species Occupancy Maps.pdf', height=6, width=8)
for(i in 1:length(species)){
	name=my.taxa[my.taxa$AOU_OUT==species[i],'PRIMARY_COM_NAME'][1]
	
print(
	spplot(sproute.data[sproute.data$Aou==species[i],],'code',
		#main=ifelse(is.na(name),species[i],paste(as.character(name),species[i],sep = ' - ')),
		col.regions=colorRampPalette(c('gray80',
		                               rgb(204/255, 88/255, 0, alpha = 1),
		                               rgb(204/255, 175/255, 192/255, alpha = 1),
		                               rgb(102/255, 102/255, 255/255, alpha = 1)))(4),
		key.space=list(title="Temporal\nOcupancy",cex.title=0.8,space="right",border=F,cex=.8),
		legendEntries=c('Absent','Transient','Moderate','Core'),
		cex = 1.25
	)
)
}
dev.off()


##############################################################
# Simple Geographic projection plot
point.colors = data.frame(code = c('A', 'T', 'M', 'C'), 
                          color = c('gray80',
                                    rgb(204/255, 88/255, 0, alpha = 1),
                                    rgb(204/255, 175/255, 192/255, alpha = 1),
                                    rgb(102/255, 102/255, 255/255, alpha = 1)),
                          pt.cex = c(.8, 1.5, 1.5, 1.5))

sproute.data3 = merge(sproute.data2, point.colors, by = 'code', all.x = T)
install.packages("maps")
library(maps)
map('state')
sp = 'X4060' #example of Red-headed Woodpecker
temp = subset(sproute.data3, Aou == sp)
points(temp$Longi, temp$Lati, cex = temp$pt.cex, col = as.character(temp$color), pch = 16)
legend("bottomright", legend = c("Core", "Intermediate", "Transient"), pch = 16, 
       pt.cex = 1.5, col = as.character(point.colors$color[4:2]))
