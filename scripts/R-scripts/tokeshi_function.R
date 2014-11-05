# Add date (from the core-transient_analysis script ... avoid plotting if you run the script for the test data)

test = out.raw[[1]]

# Inputs:

N = length(test[,1])               # The total number of species
nr = length(test[test[,2]>=.66,1]) # The number of species in the upper class
nl = length(test[test[,2]<=.66,1]) # The number of species in the lower class
h = .33333333                      # The frequency interval

# Test to determine if there is a greater number of individuals in a bin than random chance:

tokeshi.fun = function(df, N, right.or.left, h){
  outs = numeric()
  ins = seq(right.or.left:N)
  for(i in 1:length(ins)){
    outs[i] = factorial(N)/(factorial(ins[i])*factorial(N-ins[i]))*h^ins[i]*(1-h)^(N-ins[i])
  }
  prob = sum(outs)
  return(prob)
}

tokeshi.fun(test, N, 39, h) # Seems pretty darned conservative!

# Tokeshi's function to determine if the distribution is different than uniform:
# Note: Function is currently not finished ... needs to run in a nested for loop (sum of sums)

tokeshi.c.fun = function(N, i, nr, nl, j){
  (factorial(N)*h^(i + j)*(1-2*h)^(N-i-j))/(factorial(i)*factorial(j)*factorial(S-i-j))
}


