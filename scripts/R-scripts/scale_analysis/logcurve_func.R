####Tracie ref code####
# Fitting a logistic curve to EVI data (first 200 days of each year) and 
# using this to estimate greenup date:

for (r in focalrtes){

# Prairie Ridge  
subprmean = prmean[prmean$julianday %in% 1:200,]
subprmean$EVIdis = subprmean$EVImean - min(subprmean$EVImean)+.01 #possibly here to prevent error I'm now seeing where things are off by 0.01?
prlog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subprmean)
#par(mar=c(5, 4, 4, 4) + 0.1)
#plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
#     col = 'red', type = 'l', lwd = 3)
subprmean$prEVIlog = predict(prlog)+min(subprmean$EVImean)-.01
#points(prmean$julianday, prmean$prEVIlog, col = 'red', lwd = 3, 
#       lty = 'dashed', type = 'l')

# Botanical Garden
subbgmean = bgmean[bgmean$julianday %in% 1:200,]
subbgmean$EVIdis = subbgmean$EVImean - min(subbgmean$EVImean)+.01
bglog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subbgmean)
#par(mar=c(5, 4, 4, 4) + 0.1)
#plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
#     col = 'blue', type = 'l', lwd = 3)
subbgmean$bgEVIlog = predict(bglog)+min(subbgmean$EVImean)-.01
#points(bgmean$julianday, bgmean$bgEVIlog, col = 'blue', lwd = 3, 
#       lty = 'dashed', type = 'l')

# Hubbard Brook
subhbmean = hbmean[hbmean$julianday %in% 1:200,]
subhbmean$EVIdis = subhbmean$EVImean - min(subhbmean$EVImean)+.01
hblog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subhbmean)
#par(mar=c(5, 4, 4, 4) + 0.1)
#plot(hbmean$julianday, hbmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
#     col = 'blue', type = 'l', lwd = 3)
subhbmean$hbEVIlog = predict(hblog)+min(subhbmean$EVImean)-.01
#points(hbmean$julianday, hbmean$hbEVIlog, col = 'blue', lwd = 3, 
#       lty = 'dashed', type = 'l') 


# Extract greenup from logistic fit

#summary(prlog)
prgreenup.log <- summary(prlog)$coefficients["xmid","Estimate"]

#summary(bglog)
bggreenup.log <- summary(bglog)$coefficients["xmid","Estimate"]

#summary(hblog)
hbgreenup.log <- summary(hblog)$coefficients["xmid","Estimate"]  

# Extract inflection point by finding the area/aveN the meanOcc/pctCore/pctTran is half its maximum:

# Bot garden
subbgmean$inflectdiff = subbgmean$EVImean - (0.5*(max(subbgmean$EVImean)-min(subbgmean$EVImean)) + min(subbgmean$EVImean))
yhat = 0.5*(max(subbgmean$EVImean)-min(subbgmean$EVImean)) + min(subbgmean$EVImean)
y1 = subbgmean$EVImean[subbgmean$inflectdiff == tail(subbgmean$inflectdiff[subbgmean$inflectdiff <= 0], 1)]
y2 = subbgmean$EVImean[subbgmean$inflectdiff == head(subbgmean$inflectdiff[subbgmean$inflectdiff >= 0], 1)]
jd1 = subbgmean$julianday[subbgmean$inflectdiff == tail(subbgmean$inflectdiff[subbgmean$inflectdiff <= 0], 1)]
jd2 = subbgmean$julianday[subbgmean$inflectdiff == head(subbgmean$inflectdiff[subbgmean$inflectdiff >= 0], 1)]
bggreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
#figure out how to do this with regular bgmean? (got a really messed up number)

# Prairie Ridge
subprmean$inflectdiff = subprmean$EVImean - (0.5*(max(subprmean$EVImean)-min(subprmean$EVImean)) + min(subprmean$EVImean))
yhat = 0.5*(max(subprmean$EVImean)-min(subprmean$EVImean)) + min(subprmean$EVImean)
y1 = subprmean$EVImean[subprmean$inflectdiff == tail(subprmean$inflectdiff[subprmean$inflectdiff <= 0], 1)]
y2 = subprmean$EVImean[subprmean$inflectdiff == head(subprmean$inflectdiff[subprmean$inflectdiff >= 0], 1)]
jd1 = subprmean$julianday[subprmean$inflectdiff == tail(subprmean$inflectdiff[subprmean$inflectdiff <= 0], 1)]
jd2 = subprmean$julianday[subprmean$inflectdiff == head(subprmean$inflectdiff[subprmean$inflectdiff >= 0], 1)]
prgreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
#figure out how to do this with regular prmean?

# Hubbard Brook
subhbmean$inflectdiff = subhbmean$EVImean - (0.5*(max(subhbmean$EVImean)-min(subhbmean$EVImean)) + min(subhbmean$EVImean))
yhat = 0.5*(max(subhbmean$EVImean)-min(subhbmean$EVImean)) + min(subhbmean$EVImean)
y1 = subhbmean$EVImean[subhbmean$inflectdiff == tail(subhbmean$inflectdiff[subhbmean$inflectdiff <= 0], 1)]
y2 = subhbmean$EVImean[subhbmean$inflectdiff == head(subhbmean$inflectdiff[subhbmean$inflectdiff >= 0], 1)]
jd1 = subhbmean$julianday[subhbmean$inflectdiff == tail(subhbmean$inflectdiff[subhbmean$inflectdiff <= 0], 1)]
jd2 = subhbmean$julianday[subhbmean$inflectdiff == head(subhbmean$inflectdiff[subhbmean$inflectdiff >= 0], 1)]
hbgreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
#figure out how to do this with regular hbmean?

temp.dataframe = data.frame(prgreenup.half, bggreenup.half, hbgreenup.half, 
                            prgreenup.log, bggreenup.log, hbgreenup.log)

samp.dataframe = rbind(samp.dataframe, temp.dataframe)
 }# end for loop

greenup <- samp.dataframe
greenup$year <- c(2000:2016)

# Plotting
par(mar = c(2,2,3,2), mfrow = c(1,1), oma = c(2,2,2,2))
plot(greenup$year, greenup$prgreenup.log, col = 'red', type = 'l', ylim = c(70,180),
     xlab = 'Year', ylab = "Julian day of greenup", lwd = 2)
points(greenup$year, greenup$bggreenup.log, col = 'blue', type = 'l', lwd = 2)
points(greenup$year, greenup$hbgreenup.log, col = 'green3', type = 'l', lwd = 2)
points(greenup$year, greenup$prgreenup.half, col = 'red', type = 'l', lwd = 2, lty = 2)
points(greenup$year, greenup$bggreenup.half, col = 'blue', type = 'l', lwd = 2, lty = 2)
points(greenup$year, greenup$hbgreenup.half, col = 'green3', type = 'l', lwd = 2, lty = 2)
legend("topleft", c('PR logistic', 'BG logistic', 'HB logistic', 'PR half max', 'BG half max', 'HB half max'), lwd = 2, 
       lty = c(1,1,1,2,2,2), col = c('red', 'blue', 'green3', 'red', 'blue', 'green3')) 
title('Greenup 2000-2016', line = 1)





####Liang ref code####
# FUNCTION FOR DRAWING A LOGISTIC CURVE GIVEN LOGISTIC FIT COEFFICIENTS
exp.mod<-function(coefs, x){
  Asym<-plogis(coefs[1])
  xmid<-coefs[2]
  scal<-coefs[3]
  Asym/(1 + exp((xmid - x)/scal))
}

#find out what num.uniq.locs corresponds to in my own data

#mle fitting : BEGIN FITTING AND MAXIMUM LIKELIHOOD ESTIMATION OF LOGISTIC CURVE
ll.exp.con<-function(Asym,xmid,scal){
  if(xmid>max(log(bbs_allscales$area)) | xmid<min(log(bbs_allscales$area))){
    nll<- -sum( dbinom(bbs_allscales$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(bbs_allscales$area,function(x) plogis(Asym)/(1 + exp((xmid - x)/(scal)))),log=TRUE)) +
      1000 *(abs(max(bbs_allscales$area)-xmid))^2        #<-make it huge if it veers outside of constraints of x
  }
  else{
      nll<- -sum( dbinom(bbs_allscales$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(bbs_allscales$area,function(x) plogis(Asym)/(1 + exp((xmid - x)/(scal)))),log=TRUE))
    }}
  nll

nll<-numeric()
xmids<-seq(80,180,20)
coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))

for(xm in 1:length(xmids)){
  guess <- list(Asym=.6,xmid=xmids[xm],scal=1)
  fit.exp.con<- mle2(ll.exp.con, start = guess, method = "Nelder-Mead",skip.hessian=T)
  coef.mat[xm,]<-coef(fit.exp.con)
  Asym<-coef(fit.exp.con)[1]
  xmid<-coef(fit.exp.con)[2]
  scal<-coef(fit.exp.con)[3]
  nll[xm]<- -sum( dbinom(bbs_allscales$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(bbs_allscales$area,function(x) plogis(Asym)/(1 + exp((xmid - x)/(scal)))),log=TRUE))
}
best.coef<-coef.mat[order(nll)[1],] ##only takes coef from the model with the smallest neg.log.likelihood
#ADD BEST FIT LOGISTIC CURVE TO PLOT
lines(bbs_allscales$area,exp.mod(best.coef,bbs_allscales$area),col='blue') ##model result
abline(v=best.coef[2], col='red')

bbs_allscales$prop[is.nan(bbs_allscales$prop)==T] = 0                                                     
temp.x = bbs_allscales$area#[is.nan(bbs_allscales$prop)==F]
temp.prop = bbs_allscales$prop     #[is.nan(bbs_allscales$prop)==F]
temp.yr = rep(bbs_allscales$Year, length(temp.prop))

x=lm(exp.mod(best.coef, bbs_allscales$area)~temp.prop)
inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.x, temp.prop, exp.mod(best.coef, bbs_allscales$area), summary(x)$r.squared, long[i,], lat[j,], long[i+1,], lat[j+1,]))
