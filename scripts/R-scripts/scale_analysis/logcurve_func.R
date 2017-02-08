# FUNCTION FOR DRAWING A LOGISTIC CURVE GIVEN LOGISTIC FIT COEFFICIENTS
exp.mod<-function(coes,jd){
  Asym<-plogis(coes[1])
  xmid<-coes[2]
  scal<-coes[3]
  Asym/(1 + exp((xmid - jd)/scal))
}

#mle fitting : BEGIN FITTING AND MAXIMUM LIKELIHOOD ESTIMATION OF LOGISTIC CURVE
ll.exp.con<-function(Asym,xmid,scal){
  if(xmid>max(temp.data2$JulianDay)){
    nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
      1000 *(abs(max(temp.data2$JulianDay)-xmid))^2        #<-make it huge if it veers outside of constraints of jd
  }
  else{
    if(xmid<min(temp.data2$JulianDay)){
      nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
        1000 *(abs(min(temp.data2$JulianDay)-xmid))^2
    }
    else{
      nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
    }}
  nll
}
nll<-numeric()
coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))
xmids<-seq(80,180,20)
for(xm in 1:length(xmids)){
  guess <- list(Asym=.6,xmid=xmids[xm],scal=1)
  fit.exp.con<- mle2(ll.exp.con, start = guess, method = "Nelder-Mead",skip.hessian=T)
  coef.mat[xm,]<-coef(fit.exp.con)
  Asym<-coef(fit.exp.con)[1]
  xmid<-coef(fit.exp.con)[2]
  scal<-coef(fit.exp.con)[3]
  nll[xm]<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
}
best.coef<-coef.mat[order(nll)[1],] ##only takes coef from the model with the smallest neg.log.likelihood
#ADD BEST FIT LOGISTIC CURVE TO PLOT
lines(temp.data2$JulianDay,exp.mod(best.coef,temp.data2$JulianDay),col='blue') ##model result
abline(v=best.coef[2], col='red')

temp.data2$prop[is.nan(temp.data2$prop)==T] = 0                                                     
temp.jd = temp.data2$JulianDay#[is.nan(temp.data2$prop)==F]
temp.prop = temp.data2$prop     #[is.nan(temp.data2$prop)==F]
temp.yr = rep(temp.data2$Year, length(temp.prop))

x=lm(exp.mod(best.coef, temp.data2$JulianDay)~temp.prop)
inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, exp.mod(best.coef, temp.data2$JulianDay), summary(x)$r.squared, long[i,], lat[j,], long[i+1,], lat[j+1,]))

#inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, long[i,], lat[j,]))
}
