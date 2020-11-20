source("Lib.R")
source("season.R")
##################
#¤ Simuler Data ¤# 
##################
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
navn <- c("DE","FR")
for (i in 1:2) {
  
  Data = tibble(X = data[,i+1], Y = data[,i+3])

F_X = ecdf(Data$X)
F_Y = ecdf(Data$Y)


U = Data %>% mutate(U = F_X(X), V = F_Y(Y)) %>% select(U,V)
fitted = fitCopula(tCopula(),U, method="itau",start=NULL)

x_seq = seq(from = -20, to = 20, length.out = 1000)

y = 0
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
plot(x_seq, `P(X<=x|Y=y)`, type = "l", col = "green", xlab = "x",main=navn[i])

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
lines(x_seq, `P(X<=x|Y=y)`, col = "orange")


legend("bottomright", legend=c("y=0", "y=10","y=-10","y=-20","y=20"),
       col=c("green","blue", "purple","red","orange"), lty=1, cex=0.8)
  
}



