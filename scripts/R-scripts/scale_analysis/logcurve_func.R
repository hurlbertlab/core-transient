####Tracie ref code####
#----GAUSSIAN FITS FOR TABLE 1 PHENOLOGY----

fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
}

par(mfrow = c(1,1))

# First panel 1
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.36), ylab = "Mean density of caterpillars", main = '2015 Visual')

# Fit a normal curve using least squares
gfit1 = fitG(PR.LEPL15.sci$julianday, PR.LEPL15.sci$meanDensity, weighted.mean(PR.LEPL15.sci$julianday, PR.LEPL15.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit1$par
r2 = cor(PR.LEPL15.sci$julianday, p[3]*dnorm(PR.LEPL15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# First panel 2
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)
legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))

# Fit a normal curve using least squares
gfit2 = fitG(PR.LEPL15.cs$julianday, PR.LEPL15.cs$meanDensity, weighted.mean(PR.LEPL15.cs$julianday, PR.LEPL15.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit2$par
r2 = cor(PR.LEPL15.cs$julianday, p[3]*dnorm(PR.LEPL15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Second panel 1
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 Beat Sheet')

# Fit a normal curve using least squares
gfit3 = fitG(PR.LEPL16.sci$julianday, PR.LEPL16.sci$meanDensity, weighted.mean(PR.LEPL16.sci$julianday, PR.LEPL16.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit3$par
r2 = cor(PR.LEPL16.sci$julianday, p[3]*dnorm(PR.LEPL16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Second panel 2
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit4 = fitG(PR.LEPL16.cs$julianday, PR.LEPL16.cs$meanDensity, weighted.mean(PR.LEPL16.cs$julianday, PR.LEPL16.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit4$par
r2 = cor(PR.LEPL16.cs$julianday, p[3]*dnorm(PR.LEPL16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Third panel 1
PR.ORTH15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Mean density of orthopterans", main = '')

# Fit a normal curve using least squares
gfit5 = fitG(PR.ORTH15.sci$julianday, PR.ORTH15.sci$meanDensity, weighted.mean(PR.ORTH15.sci$julianday, PR.ORTH15.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit5$par
r2 = cor(PR.ORTH15.sci$julianday, p[3]*dnorm(PR.ORTH15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Third panel 2
PR.ORTH15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit6 = fitG(PR.ORTH15.cs$julianday, PR.ORTH15.cs$meanDensity, weighted.mean(PR.ORTH15.cs$julianday, PR.ORTH15.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit6$par
r2 = cor(PR.ORTH15.cs$julianday, p[3]*dnorm(PR.ORTH15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fourth panel 1
PR.ORTH16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", main = '')

# Fit a normal curve using least squares
gfit7 = fitG(PR.ORTH16.sci$julianday, PR.ORTH16.sci$meanDensity, weighted.mean(PR.ORTH16.sci$julianday, PR.ORTH16.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit7$par
r2 = cor(PR.ORTH16.sci$julianday, p[3]*dnorm(PR.ORTH16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fourth panel 2
PR.ORTH16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit8 = fitG(PR.ORTH16.cs$julianday, PR.ORTH16.cs$meanDensity, weighted.mean(PR.ORTH16.cs$julianday, PR.ORTH16.cs$meanDensity),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit8$par
r2 = cor(PR.ORTH16.cs$julianday, p[3]*dnorm(PR.ORTH16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fifth panel 1
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.5), ylab = "Mean density of bird food", main = '')

# Fit a normal curve using least squares
gfit9 = fitG(PR.BIRD15.sci$julianday, PR.BIRD15.sci$meanDensity, weighted.mean(PR.BIRD15.sci$julianday, PR.BIRD15.sci$meanDensity),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit9$par
r2 = cor(PR.BIRD15.sci$julianday, p[3]*dnorm(PR.BIRD15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fifth panel 2
PR.BIRD15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit10 = fitG(PR.BIRD15.cs$julianday, PR.BIRD15.cs$meanDensity, weighted.mean(PR.BIRD15.cs$julianday, PR.BIRD15.cs$meanDensity),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit10$par
r2 = cor(PR.BIRD15.cs$julianday, p[3]*dnorm(PR.BIRD15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 1
PR.BIRD16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.2), ylab = "", main = '')

# Fit a normal curve using least squares
gfit11 = fitG(PR.BIRD16.sci$julianday, PR.BIRD16.sci$meanDensity, weighted.mean(PR.BIRD16.sci$julianday, PR.BIRD16.sci$meanDensity),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit11$par
r2 = cor(PR.BIRD16.sci$julianday, p[3]*dnorm(PR.BIRD16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 2
PR.BIRD16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit12 = fitG(PR.BIRD16.cs$julianday, PR.BIRD16.cs$meanDensity, weighted.mean(PR.BIRD16.cs$julianday, PR.BIRD16.cs$meanDensity),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit12$par
r2 = cor(PR.BIRD16.cs$julianday, p[3]*dnorm(PR.BIRD16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


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
