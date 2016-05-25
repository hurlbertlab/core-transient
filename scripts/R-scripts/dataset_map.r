library(maps)


zoo = read.table('//bioark.bio.unc.edu/hurlbertlab/databases/continuous plankton recorder dataset/zooplankton/occurrence.txt',
                 sep='\t',header=FALSE,fill=TRUE,quote='\"')
head(zoo)

deg.res = .5

zoo$latfloor = deg.res*floor(zoo$V3/deg.res)
zoo$longfloor = deg.res*floor(zoo$V11/deg.res)
zoo$latlong = apply(zoo[,c('latfloor','longfloor')],1,function(x) paste(x[1],x[2],sep=' '))
zoo$date = zoo$V13 + 100*zoo$V9 + 10000*zoo$V10
#head(zoo)
zoositedate = unique(zoo[,c('latlong','latfloor','longfloor','date','V10')])
latlongs = unique(zoositedate[,c('latlong','latfloor','longfloor')])
latlongcount = data.frame(table(zoositedate[,c('latlong','V10')]))

survey.per.yr.threshold = 10
yrs.per.cell.threshold = 15

latlongcount2 = latlongcount[latlongcount$Freq >= survey.per.yr.threshold,]

cellyrs.good = data.frame(table(latlongcount2[,'latlong']))
names(cellyrs.good) = c('latlong','numyrs')
cellyrs.good2 = merge(cellyrs.good[cellyrs.good$numyrs >= yrs.per.cell.threshold, ], latlongs, by='latlong', all.x=T)
points(cellyrs.good2$longfloor, cellyrs.good2$latfloor, pch=16, cex=cellyrs.good2$numyrs/30, col='green')

#zoofoo = table(zoositedate$latlong)
#plot(zoofoo[order(zoofoo,decreasing=TRUE)])
#plot(zoofoo[order(zoofoo,decreasing=TRUE)], xlim = c(0,5000))
#plot(zoofoo[order(zoofoo,decreasing=TRUE)], xlim = c(0,300))

gbr = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/aims_longterm_fish_monitoring/51dc2b3ccfe29_20130709_173531.csv',
               header=TRUE,fill=TRUE,quote='\"')
gbr.latlong = unique(gbr[,c('latitude','longitude')])


pisco = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/pisco/pisco_intertidal.33.3-_intertidal.33.3',
                 header=T,fill=T,quote='\"')
pisco.sub = subset(pisco, latitude.dd. <50 & latitude.dd. > 32.7)

#NE Atlantic trawl data
trawl = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/NE Fisheries Trawl Data/51def6e83403b_20130711_202355.csv',
                 header=T,fill=T,quote='\"')
trawl$latfloor = deg.res*floor(trawl$latitude/deg.res)
trawl$longfloor = deg.res*floor(trawl$longitude/deg.res)
trawl$latlong = apply(trawl[,c('latfloor','longfloor')],1,function(x) paste(x[1],x[2],sep=' '))
trawl.uniLLyr = unique(trawl[,c('latlong','latfloor','longfloor','yearcollected')])
trawl.latlongs = unique(trawl.uniLLyr[,c('latlong','latfloor','longfloor')])
trawl.LLct = data.frame(table(trawl.uniLLyr[,c('latlong')]))
names(trawl.LLct) = c('latlong','numyrs')
trawl.yr.threshold = 10
trawl.goodcells = merge(trawl.LLct, trawl.latlongs, by='latlong',all.x=T)

#Focus on 10 year period, 1999-2008
trawl.uniLLyr99.08 = subset(trawl.uniLLyr, yearcollected >=1999)
trawl.LLct.9908 = data.frame(table(trawl.uniLLyr99.08$latlong))
names(trawl.LLct.9908) = c('latlong','numyrs')
trawl.goodcells.9908 = merge(trawl.LLct.9908, trawl.latlongs, by='latlong',all.x=T)

#NW Atlantic trawl data
#ices = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/ices biological community fish/51dc4d3e3789f_20130709_195038.csv', 
#                 header=TRUE, fill=TRUE, quote='\"')

plants = read.table('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/plant_dataset_latlongs.txt',
                  header = T, sep = '\t')
cbc = read.table('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/cbcs_latlongs_96-105.txt',
                 header = T, sep = '\t')
naba = read.table('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/NABA_2001-2005_circles.txt',
                  header = T, sep = '\t')
bbs = read.table('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/bbs_96-10_latlongs.txt',
                  header = T, sep = '\t')


#Map
pdf(paste('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/dataset_map_',Sys.Date(),'.pdf',sep=''),height=12,width=12)
# Terrestrial datasets
par(mfrow=c(2,1), mar=c(0,0,0,0))
cex.terr = 1.3

map('world',xlim=c(-165,-55),ylim=c(25,70), bg='black', fill=T, col='white')
map('state',add=T)
cbc.threshold = 1300
points(cbc$Longitude[cbc$id2 < cbc.threshold], cbc$Latitude[cbc$id2 < cbc.threshold],
       pch = 16, col = 'lightgreen', cex = cex.terr)
points(bbs$Longi, bbs$Lati, pch = 18, col = 'blue', cex = cex.terr)
points(naba$Longitude, naba$Latitude, pch = 17, col = 'yellow', cex = cex.terr)
points(plants$Longitude, plants$Latitude, pch = 15, col = 'coral', cex = 1.5*cex.terr)
legend(-165,50,legend = c('Birds - CBC', 'Birds - BBS', 'Butterflies', 'Plants'),
       pch = c(16, 18, 17, 15), col = c('lightgreen', 'blue', 'yellow', 'coral'), 
       box.col='white', cex = 2.1, bg = 'white', pt.cex = 1.2*c(cex.terr, cex.terr, cex.terr, 1.5*cex.terr))
mtext("(a)", 2, at = 70, las = 1, cex = 3, col = 'white', line = 3)

# Marine datasets
par(new=F)
map('world', ylim = c(-55,85), xlim = c(-170,170))
rect(-180, -180, 180, 180, col = 'white')
map('world', ylim = c(-55,85), xlim = c(-170,170), bg = 'white', fill = T, col = 'black', add=T)
cex.mar = 1.5
points(cellyrs.good2$longfloor, cellyrs.good2$latfloor, pch = 16, cex = cex.mar, col ='green4')
points(trawl.goodcells.9908$longfloor,trawl.goodcells.9908$latfloor, pch = 16, col = 'red', cex = cex.mar)
points(gbr.latlong$longitude, gbr.latlong$latitude, pch = 16, cex = cex.mar, col = 'orange')
points(pisco.sub$longitude.dd.,pisco.sub$latitude.dd., pch = 16, col = 'royalblue', cex = cex.mar)
cex.txt = 2.3
text(-134,19,"Rocky\nintertidal", cex=cex.txt, col = 'royalblue')
text(5,75,"Plankton", cex=cex.txt, col = 'green4')
text(-50,30,"Fish\nTrawls", cex=cex.txt, col = 'red')
text(86,-20,"Great Barrier\nReef Fish", cex=cex.txt, col = 'orange')
mtext("(b)", 3, at = -160, cex = 3, col = 'white', line = 1)
dev.off()
