source("surface/Classes.R")
source('~/P9/surface/LinearInterpolator.R')
source('~/P9/surface/Interpolations.R', encoding = 'UTF-8')
source('~/P9/surface/Common_Copulas.R')


plot(fitted@copula, surface = dCdU,n=1)


# Initialise with density
Independence = Copula(density = function(u,v){1})

# Initialise with copula
Independence_Alternative = Copula(copula = function(u,v){u*v})


#Evaluate density 
d(Independence)(0.5,0.5)

#Evaluate copula 
p(Independence)(0.5,0.5)

#Sample from the copula (here 1000 points) 
plot(r(Independence)(1000), xlab = "U", ylab = "V")


#plot the copula
plot(Independence)

#plot the density
plot(Independence, surface = dC)

#plot dC/dU
plot(Independence, surface = dCdU)

#plot dC/dV
plot(Independence, surface = dCdV)

