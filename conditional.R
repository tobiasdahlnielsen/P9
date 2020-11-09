source("Lib.R")

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

x_seq = seq(from = -5, to = 5, length.out = 1000)

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

y = 1
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "blue")

y = 2
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "purple")

y = 3
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "red")

y = 4
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "yellow")


legend("bottomright", legend=c("y=0", "y=1","y=2","y=3","y_4"),
       col=c("green","blue", "purple","red","yellow"), lty=1, cex=0.8)

