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

x_seq = seq(from = -20, to = 20, length.out = 10000)

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

y = 0
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
plot(x_seq, `P(X<=x|Y=y)`, type = "l", col = "green", xlab = "x")

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


legend("bottomright", legend=c("y=0", "y=1","y=2","y=3","y_4"),
       col=c("green","blue", "purple","red","yellow"), lty=1, cex=0.8)

