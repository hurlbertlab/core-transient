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
coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))
xmids<-seq(80,180,20)
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

#inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.x, temp.prop, long[i,], lat[j,]))

