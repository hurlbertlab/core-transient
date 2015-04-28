# Functions:

logit = function(x) log(x/(1-x))

inv.logit = function(x) exp(x)/(1+exp(x))

scale.01 = function(x){
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
}

unscale.01 = function(scaled.x){
  n = length(scaled.x)
  s = .5
  (scaled.x*n - s)/(n-1)
}


fitBeta2 = function(x) {
  shape.params = fitdistr(occs, "beta", list(shape1 = 2, shape2 = 2)))
  return(as.vector(shape.params$estimate))
}

# Testing:

x = seq(0,1,.2) ; x

z = scale.01(x) ; z

lz = logit(z) ; lz

iz = inv.logit(lz) ; iz

unscale.01(iz)



