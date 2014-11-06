# Add date (from the core-transient_analysis script ... avoid plotting if you run the script for the prop.df data)

source('scripts/R-scripts/ct_proportion_frame.R')

prop.df = props.df[[1]]

# Inputs:

N = length(prop.df[,1])               # The total number of species
nr = length(prop.df[prop.df[,2]>=.66,1]) # The number of species in the upper class
nl = length(prop.df[prop.df[,2]<=.33,1]) # The number of species in the lower class
h = .33333333                      # The frequency interval

# prop.df to determine if there is a greater number of individuals in a bin than random chance (F>f):

tokeshi.fun = function(df, N, right.or.left, h){
  outs = NULL
  ins = right.or.left:N
  for(i in ins){
    outs[i] = (factorial(N)/(factorial(i)*factorial(N-i)))*h^i*(1-h)^(N-i)
  }
  return(sum(na.omit(outs)))
}

tokeshi.fun(prop.df, N,nl, h) 

# Plotting Tokeshi function to look at # of species per bin:
# The .05 cut-off signifies a strong mode, .25 a mode, and .5 a weak mode
# For bimodality both modes must be at least weak:

h = .3333
x = 1:45
y = numeric()
for(i in x){
  y[i] = tokeshi.fun(prop.df, N, i, h)
}

plot.new()
plot(x,y, xlab = '# of species', ylab = 'P(F>f)', type = 'l', lwd = 2)
abline(h = .05,lty =4)
abline(h = .25,lty =2)
abline(h = .5,lty =3)

# Tokeshi's function to determine if the distribution is different than uniform:
# Note: Function is currently not finished ... needs to run in a nested for loop (sum of sums)

tokeshi.u.fun = function(N, nr, nl, h){
  ins.i = nl:N 
  ins.j = nr:N
  outs = NULL
  for (i in ins.i){
    for(j in ins.j){
      outs[j] = (factorial(N)*h^(i + j)*(1-2*h)^(N-i-j))/
            (factorial(i)*factorial(j)*factorial(S-i-j))
      }} 
  sum(na.rm(outs)
  }

tokeshi.u.fun(N, nr, nl, h)

ins.i
ins.j
out1 = NULL

for(i in ins.i){
  for(j in ins.j){
    print(sum(i,j))
  }
}