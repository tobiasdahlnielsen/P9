source("Lib.R")
source("season.R")
##################
#¤ Simuler Data ¤# 
##################


Data = tibble(X = data$DE, Y = data$DEForecast)
Data %>% ggplot(aes(x = X, y = Y)) + geom_point()


##################
#¤Fit Marginaler¤# 
##################

F_X = ecdf(Data$X)
F_Y = ecdf(Data$Y)

##################
#¤  Fit Copula  ¤# 
##################

U = Data %>% mutate(U = F_X(X), V = F_Y(Y)) %>% select(U,V)
U %>% ggplot(aes(x = U, y = V)) + geom_point()
fitted = fitCopula(tCopula(),U, method="itau",start=NULL)


x_seq = seq(from = -20, to = 20, length.out = 1000)

C_hat = function(w){
  pCopula(w, fitted@copula)
}

`F_X|Y` = function(x,y,F_X,F_Y,C){
  eps = 1e-5
  
  v = F_Y(y)
  
  #Numerisk afledt i punktet
  dCdV = function(u){
    (C(c(u,v+eps)) - C(c(u,v-eps)))/(2*eps)
  }
  
  sapply(X = F_X(x), dCdV)
}
y=c(-20,-15,-10,-5,0,5,10,15,20)*3
y=seq(from = -20, to = 20, length.out = 1000)
cdfm <- c()
for (i in 1:length(y)) {
  cdfm = cbind(cdfm,`F_X|Y`(x_seq,y[i],F_X,F_Y,C_hat))
}


persp3D(x=y,y=c(1:1000),z=t(cdfm))

y = 0
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
plot(x_seq, `P(X<=x|Y=y)`, type = "l", col = "green", xlab = "x")

# dxdv = function(v){
#   nyv <- c()
#   for (i in 2:(length(v)-1)) {
#   nyv[i-1] <- (v[i+1]-v[i-1])/(2*diff(x_seq)[1])
#   }
#   return(nyv)
# }
# plot(dxdv(`P(X<=x|Y=y)`),type="l")
# 

y = 10
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "blue")

y = -10
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "purple")

y = -20
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "red")

y = 20
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "yellow")


legend("bottomright", legend=c("y=0", "y=1","y=2","y=3","y=4"),
       col=c("green","blue", "purple","red","yellow"), lty=1, cex=0.8)





`d/dx` = function(f, eps = 1e-6){
  return(function(x){(f(x + eps) - f(x-eps))/(2*eps)})
}

x_prime = seq(from = -6,to = 6, length.out = 75)

F_X = approxfun(x_prime, F_X(x_prime),rule = 2)
F_Y = approxfun(x_prime, F_Y(x_prime),rule = 2)

f_X = `d/dx`(F_X)
f_Y = `d/dx`(F_Y)


{plot(seq(from = -6,to = 6, length.out = 100),f_X(seq(from = -6,to = 6, length.out = 100)),type = "l", xlab = "x", ylab = "")
  lines(seq(from = -6,to = 6, length.out = 100),f_Y(seq(from = -6,to = 6, length.out = 100)),col = "red")
  legend("topright", legend=c("f_X", "f_Y"),
         col=c("black","red"), lty=1, cex=0.8)}



`f_X|Y` = function(x,y,cop,F_X,F_Y,f_X){cop(c(F_X(x),F_Y(y)))*f_X(x)}

x = seq(from = -6, to = 6, length.out = 1000)

plot(x,sapply(x, `f_X|Y`, y = 0, cop = c_hat, F_X = F_X, F_Y =F_Y, f_X = f_X), type = "l",col = "green", ylab = "f_{X|Y=y}(x)", ylim = c(0,1.3))
lines(x_prime,sapply(x_prime, `f_X|Y`, y = 1, cop = C_hat, F_X = F_X, F_Y =F_Y, f_X = f_X), col = "blue")
lines(x_prime,sapply(x_prime, `f_X|Y`, y = 2, cop = c_hat, F_X = F_X, F_Y =F_Y, f_X = f_X), col = "purple")
lines(x_prime,sapply(x_prime, `f_X|Y`, y = 3, cop = c_hat, F_X = F_X, F_Y =F_Y, f_X = f_X), col = "red")
legend("topleft", legend=c("y=0", "y=1","y=2","y=3"),
       col=c("green","blue", "purple","red"), lty=1, cex=0.8)
