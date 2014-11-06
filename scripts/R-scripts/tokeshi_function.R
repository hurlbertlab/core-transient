# Add date (from the core-transient_analysis script ... avoid plotting if you run the script for the test data)

test = out.raw[[1]]

# Inputs:

N = length(test[,1])               # The total number of species
nr = length(test[test[,2]>=.66,1]) # The number of species in the upper class
nl = length(test[test[,2]<=.33,1]) # The number of species in the lower class
h = .33333333                      # The frequency interval

# Test to determine if there is a greater number of individuals in a bin than random chance:

tokeshi.fun = function(df, N, right.or.left, h){
  outs = NULL
  ins = right.or.left:N
  for(i in ins){
    outs[i] = (factorial(N)/(factorial(i)*factorial(N-i)))*h^i*(1-h)^(N-i)
  }
  return(sum(na.omit(outs)))
}

tokeshi.fun(test, N,nl, h) 

# Plotting Tokeshi function to look at # of species per bin:
# The .05 cut-off signifies a strong mode, .25 a mode, and .5 a weak mode
# For bimodality both modes must be at least weak:

h = .3333
x = 1:45
y = numeric()
for(i in x){
  y[i] = tokeshi.fun(test, N, i, h)
}

plot.new()
plot(x,y, xlab = '# of species', ylab = 'P(F>f)', type = 'l', lwd = 2)
# abline(v = h*N,lty =2)
abline(h = .05,lty =4)
abline(h = .25,lty =2)
abline(h = .5,lty =3)



# Tokeshi's function to determine if the distribution is different than uniform:
# Note: Function is currently not finished ... needs to run in a nested for loop (sum of sums)

tokeshi.u.fun = function(N, nr, nl, h){
  ins.i = nl:N 
  ins.j = nr:N
  outs = 0
  for (i in ins.i){
    for(j in ins.j){
      outs = outs + (factorial(N)*h^(ins.i[i] + ins.j[j])*(1-2*h)^(N-ins.i[i]-ins.j[j]))/
            (factorial(ins.i[i])*factorial(ins.j[j])*factorial(S-ins.i[i]-ins.j[j]))
      }}
    outs
  }

tokeshi.u.fun(N, nr, nl, h)


