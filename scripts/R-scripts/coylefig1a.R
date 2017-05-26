occupancy.matrix = read.csv("site_sp_occupancy_matrix.csv", header = TRUE)
coyle_long = gather(coyle, "AOU", "occ", X2881:X22860)
coyle_long$AOU = substring(coyle_long$AOU, 2)
coyle_long = na.omit(coyle_long)
coyle_long$stateroute = coyle_long$X
density(coyle_long$occ)
par(mar=c(4,4,1,1)+0.5)
par(lend=2)
num.years = 15

pdf('output/plots/coyle.pdf', height = 8, width = 10)
# Add kernel density
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
axis(1,at=seq(0,1,0.1),pos=0,lwd=2, font=2, cex.axis=2)
axis(2,at=seq(0,2.5,0.5),labels=c(NA,seq(0.5,2.5,0.5)),las=2,pos=0,lwd=2, font=2, cex.axis=2)

# Add titles
title(main='',xlab='Proportion of time present at site',ylab='Density of species-sites',
      line=2,cex.lab=2.25)

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
