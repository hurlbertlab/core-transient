library(MASS)

# Scale occupancy from [0,1] to (0,1) following Smithson and Verkuilen 2006
# Note: See supplemental at http://supp.apa.org/psycarticles/supplemental/met_11_1_54/met_11_1_54_supp.html

occs.scaled = function(dataID){
  x = out.list[[1]][[dataID]]$prop.yrs
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
  }

fitbeta = function(dataID) {
  occs  = occs.scaled(dataID)
  shape.params = suppressWarnings(fitdistr(occs, "beta", list(shape1 = 2, shape2 = 2)))
  return(as.vector(shape.params$estimate)) 
  }

plot.beta = function(dataID, color) {
  shape.params =fitbeta(dataID)
  par(new=T)
  beta.dist = rbeta(1000, shape1 = shape.params[1], shape2 = shape.params[2])
  out.list[[3]][[dataID]]
  plot(density(beta.dist), bty = 'n',xlim = c(0,1), yaxt="n", xaxt="n", ylab="", xlab="", main="", col = color)
  }

out.list[[3]][[12]]
plot.beta(12,'red')
fitbeta(12)

out.list[[3]][[1]]
plot.beta(1,'red')
fitbeta(1)

out.list[[3]][[2]]
plot.beta(2,'red')
fitbeta(2)

out.list[[3]][[3]]
plot.beta(3,'red')
fitbeta(3)


