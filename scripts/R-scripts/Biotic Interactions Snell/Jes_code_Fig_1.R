## For all years a route was surveyed
sproute = table(countsAll$stateroute,countsAll$Aou)
yroute = table(countsAll$stateroute,countsAll$Year)
num.years = apply(yroute, 1, function(x) length(x[x>0]))

# Core-transient species lists
core.list = sapply(1:nrow(sproute), function(x) find.core(sproute[x,],num.years[x],c(0.3334,0.6666)))
cores = core.list[1,]
names(cores) = rownames(sproute)
occas = core.list[2,]
names(occas) = rownames(sproute)
core.list = cbind(core = cores, occa = occas)

# A matrix of occupancies for each species on each route
use.sproute = sproute[,colnames(sproute) %in% unique(counts$Aou)]
occupancy.matrix = apply(use.sproute,2,function(x) x/num.years)
occupancy.matrix[occupancy.matrix==0]<-NA


## Number of species never classified as core
nc.mat = occupancy.matrix>0.6666
length(which(apply(nc.mat, 2, function(x) sum(x, na.rm=T))==0))

########################################################################
### Calculate Richness ###

# The following scripts were run on the Emerald computing cluster at UNC Chapel Hill to calculate richness:
#  calc_rich_b=0.66.txt, calc_rich_b=0.75, calc_rich_b=0.5

# Output were saved as 'core-transient_rich_b=0.66.csv', 'core-transient_rich_b=0.75.csv', 'core-transient_rich_b=0.5.csv'
# Files were read in at the top of this script.


########################################################################
### Richness - Environment Models ###

# Define richness dataset and time window and merge with environmental data
#  This removes AK plots for which environmental data was not calculated
use.birds = merge(subset(rich66, t==15), env.noNA)
use.birds = merge(subset(rich75, t==15), env.noNA)
use.birds = merge(subset(rich50, t==15), env.noNA)


# Define variable types
mean.vars = grep('mean',names(use.birds))
var.vars = grep('var',names(use.birds))
response.vars = which(names(use.birds) %in% c('R','Rcore','Rocca')) # For use when use.birds is made with abun.rich

# Standardize variables by mean and stddev
birds.std = use.birds
birds.std[,mean.vars] = apply(use.birds[,mean.vars],2,standard.func)
birds.std[,var.vars] = apply(use.birds[,var.vars],2,function(x) standard.func(log(x)))
birds.std[,response.vars] = apply(use.birds[,response.vars],2,standard.func)
birds.std[,'regR'] = standard.func(birds.std$regR)

# Make set of models to run
mods = expand.grid(response.vars,c(mean.vars,var.vars,grep('regR', names(birds.std))))

# Calculate univariate models
lms = apply(mods,1,function(x){
  lm(birds.std[,x[1]]~birds.std[,x[2]])		
})

mod.coefs = as.data.frame(t(sapply(lms, coef)))
names(mod.coefs)[1:2] = c('intercept','slope')
mod.coefs$response = names(birds.std)[mods[,1]]
mod.coefs$predictor = names(birds.std)[mods[,2]]	
mod.coefs$R2 = sapply(lms,function(x) summary(x)$r.squared)
mod.coefs$p = sapply(lms,function(x) summary(x)$coefficients[2,4])
mod.coefs$std.err = sapply(lms,function(x) summary(x)$coefficients[2,2])
mod.coefs$t = sapply(lms,function(x) summary(x)$coefficients[2,3])
mod.coefs$low95 = mod.coefs$slope +qnorm(0.025)*mod.coefs$std.err
mod.coefs$up95 = mod.coefs$slope + qnorm(0.975)*mod.coefs$std.err

# Store model coefficients in a variable indicating which breakpoints were used to calculate richness
mod.coefs->b.5coefs
mod.coefs->b.66coefs
mod.coefs->b.75coefs

## Correlation between core and transient richness
cor(birds.std$Rcore, birds.std$Rocca)

### PCA of twelve environmental variables ###

allpca = princomp(birds.std[,twelvevars])
allpca$loadings[1:length(twelvevars),1:length(twelvevars)] -> pcaloadings
colnames(allpca$scores) = sapply(1:ncol(allpca$scores), function(x) paste('pca',x,sep=''))

PCA = data.frame(allpca$scores)


### Multivariate models ###

# PCA models
lm.O1 = lm(birds.std$Rocca~PCA$pca1 + PCA$pca2) # allpca and birds.std in same order of routes
lm.C1 = lm(birds.std$Rcore~PCA$pca1 + PCA$pca2)

# NDVI - Elevation Variance models
lm.O2 = lm(Rocca~sum.NDVI.mean + elev.var, data=birds.std)
lm.C2 = lm(Rcore~sum.NDVI.mean + elev.var, data=birds.std)

# NDVI - Regional richness models
lm.O3 = lm(Rocca~sum.NDVI.mean + regR, data=birds.std)
lm.C3 = lm(Rcore~sum.NDVI.mean + regR, data=birds.std)


##################################################################################
### Figures ###

## Define dataset to use
mod.coefs = b.66coefs  # b.66.1996coefs , b.75coefs,abuncoefs10 , abuncoefs6 , b.5coefs
use.birds = merge(subset(rich66, t==15), env.noNA)
birds.std = use.birds
birds.std[,mean.vars] = apply(use.birds[,mean.vars],2,standard.func)
birds.std[,var.vars] = apply(use.birds[,var.vars],2,function(x) standard.func(log(x)))
birds.std[,response.vars] = apply(use.birds[,response.vars],2,standard.func)
birds.std[,'regR'] = standard.func(birds.std$regR)


## Define objects needed to plot maps
library(sp); library(rgdal)
prj.string<-paste("+proj=laea +lat_0=",mean(range(use.birds$Lati))," +lon_0=",mean(range(use.birds$Longi))," +units=km",sep='')

# Make a spatial data frame
routes = use.birds
coordinates(routes)=c('Longi','Lati')
proj4string(routes) = CRS("+proj=longlat +ellps=WGS84")
routes.laea = spTransform(routes, CRS(prj.string))

# Draw circles around all routes
circs = sapply(1:nrow(routes.laea), function(x){
  circ =  make.cir(routes.laea@coords[x,],40)
  circ = Polygons(list(circ),ID=routes.laea@data$stateroute[x])
}
)
circs.sp = SpatialPolygons(circs, proj4string=CRS(prj.string))

# Make North America outline
# NOTE: This shapefile is NOT provided in the Dryad repository
OUTLINES = readOGR('C:/Users/jrcoyle/Documents/UNC/GIS shape files/N Am Outline','na_base_Lambert_Azimuthal')
OUTLINES.laea = spTransform(OUTLINES,CRS(prj.string))

# Make background water
water.box = expand.grid(bbox(OUTLINES.laea)[1,],bbox(OUTLINES.laea)[2,])[c(1,2,4,3),]
water.box = rbind(water.box, water.box[1,])
water=Polygon(water.box, hole=F)
waters = Polygons(list(water),'water')
water.sp = SpatialPolygons(list(waters), proj4string=CRS(prj.string))

# Data frame
plot.data = use.birds
rownames(plot.data) = plot.data$stateroute
circs.birds = SpatialPolygonsDataFrame(circs.sp,plot.data)

# Extent of map
maplims.laea=cbind(c(-2700,2500),c(-1600,1850)) #columns: x, y 


############################
### Figure 1 ###


pdf('Figure_1.pdf',width=6,height=6)

par(mar=c(4,4,1,1)+0.5)
par(lend=2)
# Add kernal density
partdensity = density(occupancy.matrix[occupancy.matrix>0],from=1/min(num.years),
                      to=(min(num.years)-1)/min(num.years),kernel='gaussian', na.rm=T, n=2000)
plot(partdensity$y~partdensity$x,
     main='',
     xlab='',
     ylab='',
     xlim=c(0,1),ylim=c(0,2.5),
     lwd=5,axes=F,type='l',lend=2
)

# Add breakpoints
segments(0.33,0,0.33,partdensity$y[which(round(partdensity$x,2)==0.33)[1]], lty=3,lwd=2) #v2
segments(0.66,0,0.66,partdensity$y[which(round(partdensity$x,2)==0.66)[1]], lty=3,lwd=2) #v2

# Add axes
axis(1,at=seq(0,1,0.1),pos=0,lwd=2, font=2, cex.axis=1.5)
axis(2,at=seq(0,2.5,0.5),labels=c(NA,seq(0.5,2.5,0.5)),las=2,pos=0,lwd=2, font=2, cex.axis=1.5)

# Add titles
title(main='',xlab='Proportion of time present at site',ylab='Density of species-sites',
      line=2,cex.lab=1.8)

# Add proportions
allsp = !is.na(occupancy.matrix)
coresp = occupancy.matrix>=0.6667
occasp = occupancy.matrix<0.3334

text(0.66+(0.33/2),0.15,paste('(',round(sum(coresp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
     cex=1.5,font=1)
text(0.33/2,0.15,paste('(',round(sum(occasp,na.rm=T)/sum(allsp,na.rm=T)*100,1),' %)',sep=''),
     cex=1.5,font=1)

text(0.66+(0.33/2),0.35,'Core',cex=1.5,font=2) #v3
text(0.33/2,0.35,'Transient',cex=1.5,font=2) #v3

dev.off()


################## End Figure 1

###############################
### Figure 2 ###

mod.table = subset(mod.coefs, response %in% c('Rcore','Rocca'))

mod.table = mod.table[order(mod.table$response),]

# For use in never core figures
#mod.table= subset(b.66coefs, response %in% c('Rcore','R.ncore'))
#mod.table = mod.table[order(mod.table$response, decreasing=T),]

sig.table = unstack(mod.table, slope~predictor)
rownames(sig.table)=c('Rcore','Rocca')
sig.table=t(sig.table)
err.table=unstack(mod.table,std.err~predictor)
rownames(err.table)=c('err.core','err.occa')
err.table=t(err.table)
sig.table=cbind(sig.table,err.table)
sig.table=as.data.frame(sig.table)
sig.table$var.type = sapply(rownames(sig.table), get.varType)
sig.table$color = ifelse(sig.table$var.type==1,mycolors[1],mycolors[2])
sig.table$up99.core = sig.table$Rcore + qnorm(0.995)*sig.table$err.core
sig.table$low99.core = sig.table$Rcore + qnorm(0.005)*sig.table$err.core
sig.table$up99.occa = sig.table$Rocca + qnorm(0.995)*sig.table$err.occa
sig.table$low99.occa = sig.table$Rocca + qnorm(0.005)*sig.table$err.occa

usevars = twelvevars

use.data = sig.table[which(rownames(sig.table) %in% usevars),]

pdf("Figure_2_1996.pdf", width=6, height=6)

cairo_ps('F2.eps', width=6, height=6)
par(mar=c(4,5,1,1)+0.5)
par(lend=2)
plot(Rcore~Rocca, data = use.data, 
     xlab='Effect on transient species richness', ylab='',
     main='',xlim=c(-0.6,0.6),ylim=c(-0.54,0.8),type='n',cex.lab=1.3,axes=F,font.lab=2)
title(ylab='Effect on core species richness', line=3.5,cex.lab=1.3,font.lab=2)

abline(h=0,v=0,lty=2,col='grey60', lwd=2)
arrows(use.data$low99.occa,use.data$Rcore,lwd=2,
       use.data$up99.occa,use.data$Rcore, 
       code=3,angle=90,length=0.03, col=use.data$color)
arrows(use.data$Rocca,use.data$low99.core,lwd=2,
       use.data$Rocca,use.data$up99.core, 
       code=3,angle=90,length=0.03, col=use.data$color)

points(Rcore~Rocca, data = use.data,
       bg=use.data$color,
       pch=ifelse(use.data$var.type==1,21,24), cex=1.8)

quadpos = function(x,a,b){
  positions = list(c(-a,-b),c(1+a,-b),c(1+a,1+b),c(-a,1+b))
  positions[[x]]
} 

for(i in 1:length(usevars)){
  text(use.data$Rcore[i]~use.data$Rocca[i], 
       labels=expression('AP',AP[sigma^2],'Elev',Elev[sigma^2],'MAT',MAT[sigma^2],
                         P[max],P[min],'NDVI',NDVI[sigma^2],T[max],T[list(max,sigma^2)])[i],
       adj=quadpos(c(3,2,2,1,2,3,4,4,3,4,4,4)[i],.1,.2),
       cex=1.4,font=2)
}

#Add Regional Richness
regrow = sig.table[rownames(sig.table)=='regR',]

arrows(regrow$low99.occa,regrow$Rcore,lwd=2,
       regrow$up99.occa,regrow$Rcore, 
       code=3,angle=90,length=0.03, col='black')
arrows(regrow$Rocca,regrow$low99.core,lwd=2,
       regrow$Rocca,regrow$up99.core, 
       code=3,angle=90,length=0.03, col='black')
points(Rcore~Rocca, data = regrow, bg='grey30',
       pch=22, cex=1.8)
text(regrow$Rcore~regrow$Rocca, labels=c(expression(R[reg])),
     adj=quadpos(2,.1,.2),cex=1.4,font=2)

legend('topright', pch=c(21,24),pt.bg=mycolors,bty='n', pt.cex=1.5,cex=1.2,
       c('Local environment', 'Spatial heterogeneity'))

axis(1,lwd=2,cex.axis=1.4, font=2)
axis(2, las=1, lwd=2,cex.axis =1.4,font=2, at= seq(-0.5,0.7,.2))
box(lwd=2)
dev.off()

################## End Figure 2

###############################
### Figure 3 ###

usevars = twelvevars

# For use in Panel 1
PCA = data.frame(allpca$scores)
PCA$stateroute = birds.std$stateroute
PCA = merge(birds.std, PCA, by='stateroute', all.x=T)

LEGEND_LOC=2.5

#pdf('Figure_3_nc.pdf', height=4, width=10)
#pdf('Figure_3_abunEQ.pdf', height=4, width=10)
#pdf('Figure_3_0.75.pdf', height=4, width=10)
#pdf('Figure_3_0.5.pdf', height=4, width=10)
#pdf('Figure_3.pdf', height=4, width=10)
#pdf('Figure_3_1996.pdf', height=4, width=10)

cairo_ps("F3.eps", height=4, width=10)

par(mfrow=c(1,3))

### Panel 1

lm.O1 = lm(Rocca~pca1, data=PCA)
lm.O2 = lm(Rocca~pca2, data=PCA)
lm.O = lm(Rocca~pca1+pca2, data=PCA)

lm.C1 = lm(Rcore~pca1, data=PCA)
lm.C2 = lm(Rcore~pca2, data=PCA)
lm.C = lm(Rcore~pca1+pca2, data=PCA)

vpartcols = cbind(Core = part.variance(list(Both=lm.C, PC2 = lm.C2, PC1 = lm.C1)), 
                  Transient = part.variance(list(Both=lm.O, PC2 = lm.O2, PC1 = lm.O1)))
vpartcols[which(vpartcols<0)]=0

par(mar=c(3,5.5,3,1)+0.5)
par(lend=2)
barplot(vpartcols,
        col=c(mycolors[1],'grey',mycolors[2]), names.arg=c('Core','Transient'),axes=F,ylim=c(0,0.62),
        ylab='',cex.lab=1.3, cex.names=2, space=0.3, xlim=c(0,2.7))
title(ylab='Variance Explained', line=4, cex.lab=2)
legend(LEGEND_LOC-.5,.62,c('Spatial \nheterogeneity','Local \nenvironment'), col=c(mycolors[2:1]), 
       pch=15,  bty='n', cex=1.8, xjust=1, text.width=0.6, y.intersp = 1.5) #  horiz=T, text.width=.6
axis(2,las=1, at=seq(0,0.6,0.1), cex.axis=1.8, font=2, lwd=3)

mtext("A",3,.5,adj=0, font=2, cex=2)

### Panel 2
lm.O = lm(Rocca~sum.NDVI.mean+elev.var, data=birds.std)
lm.O1 = lm(Rocca~sum.NDVI.mean, data=birds.std)
lm.O2 = lm(Rocca~elev.var, data=birds.std)

lm.C = lm(Rcore~sum.NDVI.mean+elev.var, data=birds.std)
lm.C1 = lm(Rcore~sum.NDVI.mean, data=birds.std)
lm.C2 = lm(Rcore~elev.var, data=birds.std)

vpartcols = cbind(Core = part.variance(list(Both = lm.C,NDVI = lm.C1,Elevation = lm.C2)),
                  Occasional=part.variance(list(lm.O,lm.O1,lm.O2)))
vpartcols[which(vpartcols<0)]=0

par(mar=c(3,5.5,3,1)+0.5)
par(lend=2)
barplot(vpartcols,
        col=c(mycolors[1],'grey',mycolors[2]), names.arg=c('Core','Transient'),axes=F,ylim=c(0,0.62),
        ylab='',cex.lab=1.3, cex.names=2, space=0.3, xlim=c(0,2.7))
#title(ylab='Variance Explained', line=4, cex.lab=1.8)
legend(LEGEND_LOC,.62,c('Elevation \nvariance','NDVI'), col=c(mycolors[2:1],'grey'), 
       pch=15,  bty='n', cex=1.8, xjust=1, text.width=.6, y.intersp = 1.5) # horiz=T, text.width=c(.6,.7,.6)
axis(2,las=1, at=seq(0,0.6,0.1), cex.axis=1.8, font=2, lwd=3)

mtext("B",3,.5,adj=0, font=2, cex=2)

### Panel 3

lm.O = lm(Rocca~sum.NDVI.mean+regR, data=birds.std)
lm.O1 = lm(Rocca~sum.NDVI.mean, data=birds.std)
lm.O2 = lm(Rocca~regR, data=birds.std)

lm.C = lm(Rcore~sum.NDVI.mean+regR, data=birds.std)
lm.C1 = lm(Rcore~sum.NDVI.mean, data=birds.std)
lm.C2 = lm(Rcore~regR, data=birds.std)

vpartcols = cbind(Core = part.variance(list(Both = lm.C,NDVI = lm.C1,RegionalR = lm.C2)),
                  Occasional=part.variance(list(lm.O,lm.O1,lm.O2)))
vpartcols[which(vpartcols<0)]=0

par(mar=c(3,5.5,3,1)+0.5)
par(lend=2)
barplot(vpartcols,
        col=c(mycolors[1],'grey',mycolors[2]), names.arg=c('Core','Transient'),axes=F,ylim=c(0,0.62),
        ylab='',cex.lab=1.3, cex.names=2, space=0.3, xlim=c(0,2.7))
#title(ylab='Variance Explained', line=4, cex.lab=1.8)
legend(LEGEND_LOC,.62,c('Regional \nrichness','NDVI'), col=c(mycolors[2:1]), 
       pch=15,  bty='n', cex=1.8, xjust=1, text.width=.6, y.intersp = 1.5) #horiz=T, text.width=c(.6,.65,.6)
axis(2,las=1, at=seq(0,0.6,0.1), cex.axis=1.8, font=2, lwd=3)

mtext("C",3,.5,adj=0, font=2, cex=2)

dev.off()



###############################
### Figures not included in paper



## Plot occupancy distributions for all sites
pdf('Occupancy distributions for each route.pdf', height=6, width=6)
for(i in 1:nrow(occupancy.matrix)){
  partdensity = density(occupancy.matrix[i,],from=1/num.years[i],
                        to=(num.years[i]-1)/num.years[i],kernel='gaussian', na.rm=T, n=2000)
  plot(partdensity$y~partdensity$x,
       main=paste('Route',rownames(occupancy.matrix)[i]),
       xlab='Number of Species',
       ylab='Temporal Occupancy',
       xlim=c(0,1),ylim=c(0,2.5),
       lwd=2,axes=T,type='l',lend=2
  )
  
  #hist(occupancy.matrix[i,], xlab='Temporal Occupancy', ylab='Number of Species',
  #	main=paste('Route',rownames(occupancy.matrix)[i])
  #)
}
dev.off()



##################
### Figure S5 ###

med.abun = log10(median(abundances, na.rm=T))

medabunfac = factor(abundances >= 10^med.abun)
medoccfac = factor(occupancy.matrix >=0.5)
tab4 = xtabs(~medabunfac+medoccfac)

EQabunfac = factor(ifelse(abundances>abunCuts[2], 3, ifelse(abundances<abunCuts[1],1,2)))
thirdoccfac = factor(ifelse(occupancy.matrix>=0.6667, 3, ifelse(occupancy.matrix<0.3334,1,2)))
tabEQ = xtabs(~EQabunfac+thirdoccfac)


occumatrix = read.csv('occumatrix.csv', header = TRUE)
occumatrix1= spread(occumatrix, Species, FocalOcc)

#abun=gather(abundances, "Species", "Occ", 2881:22860)
#abun = subset(abundances, Aou == occumatrix$Species)


pdf('Figure_S5.pdf', height=7, width=14)
#par(mfrow=c(1,2))

par(mar=c(4.5,6,3,3))
plot(log10(abundances), occupancy.matrix, xlab= 'Mean abundance', ylab='',
     pch=16, col=rgb(0,.2,.5,.05), axes=F, cex.lab=1.8)
axis(1, at = c(0,med.abun,1,2,3), labels = c(1,3.6,10,100,1000), cex.axis=1.25)
axis(2, las=1, cex.axis=1.25)
title(ylab='Temporal occupancy', line=4.5, cex.lab=1.8)
box()

abline(h=0.5, col='black', lwd=1, lty=1, lend=1)
abline(v=med.abun, col='black',lwd=1, lty=1,lend=1)

text(c(.2,2,.2,2),c(.2,.2,.8,.8),labels=tab4, font=2)

#par(mar=c(4.5,6,3,3)); par(xpd=F)
#plot(log10(abundances), occupancy.matrix, xlab= 'Mean abundance', ylab='',
#	pch=16, col=rgb(0,.2,.5,.05), axes=F, cex.lab=1.8)
#axis(1, at = c(0,log10(abunCuts),1,2,3), labels = c(1,2,3.9,10,100,1000), cex.axis=1.25)
#axis(2, las=1, cex.axis=1.25)
#title(ylab='Temporal occupancy', line=4.5, cex.lab=1.8)
#box()

#abline(h=c(.3334,.6667), col='black', lwd=1, lty=1,lend=1)
#abline(v=log10(abunCuts), col='black', lwd=1, lty=1,lend=1)

#text(rep(c(.08,.44,2),3),rep(c(.15,.5,.85),each=3),labels=tabEQ, font=2)
mtext(c("Rare","","Common"), side=3, line=1, at=c(.08,.44,2), font=2, cex=1.8)
#mtext(rowSums(tabEQ), side=3, line=0, at=c(.08,.44,2), font=2, col=rgb(0,.2,.5))
par(xpd=T)
text(rep(par("usr")[2] + 0.25, 3), c(.15,.5,.85), c("Transient","","Core"),font=2, cex=1.8,srt=270)
#text(rep(par("usr")[2] + 0.1, 3), c(.15,.5,.85), colSums(tabEQ),font=2, col=rgb(0,.2,.5),srt=270)
par(xpd=F)
dev.off()

################