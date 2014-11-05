# Add date (from the core-transient_analysis script ... avoid plotting if you run the script for the test data)

test = out.raw[[1]]

# Inputs:

N = length(test[,1])               # The total number of species
nr = length(test[test[,2]>=.66,1]) # The number of species in the upper class
nl = length(test[test[,2]<=.33,1]) # The number of species in the lower class
h = .33333333                      # The frequency interval

# Test to determine if there is a greater number of individuals in a bin than random chance:

tokeshi.fun = function(df, N, right.or.left, h){
  outs = numeric()
  ins = right.or.left:N
  for(i in 1:length(ins)){
    outs[i] = (factorial(N)/(factorial(ins[i])*factorial(N-ins[i])))*h^ins[i]*(1-h)^(N-ins[i])
  }
  prob = sum(outs)
  return(prob)
}

tokeshi.fun(test, N,nl, h) 

h = .3333
x = 1:45
y = numeric()
for(i in x){
  y[i] = tokeshi.fun(test, N, i, h)
}

plot.new()
plot(x,y, type = 'l')
abline(v = h*N,lty =2)

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


