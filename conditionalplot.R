source("Lib.R")
source("season.R")
##################
#¤ Simuler Data ¤# 
##################

Data = tibble(X = data$DE, Y = data$DEForecast)
Data %>% ggplot(aes(x = X, y = Y)) + geom_point()+xlim(-80,80)+ylim(-80,80)


##################
#¤Fit Marginaler¤# 
##################

F_X = ecdf(Data$X)
F_Y = ecdf(Data$Y)

##################
#¤  Fit Copula  ¤# 
##################
dseq <- c(2,4,3,5)
par(mfrow=c(1,2))
for (i in 1:4) {
  hist(data[,dseq[i]], prob=T, col="skyblue2",breaks=100,
       main=paste("Density of",names(data)[dseq[i]]),xlab="")
  lines(density(data[,dseq[i]]), type="l", col="red", lwd=2)  # type is 'ell', not 'one'
  curve(dnorm(x, 0, sd(data[,dseq[i]])), add=T, lty="dotted")
  #legend("topright", legend=c("edf","normal"),
  #       col=c("red","black"), lty=c(1,2), cex=1)
}



par(mfrow=c(1,1))


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

x_seq = seq(from = -100, to = 100, length.out = 1000)

y = 0
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
plot(x_seq, `P(X<=x|Y=y)`, type = "l", col = "green", xlab = "x",main=navn[i])

y = 25
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "blue")

y = -25
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "purple")

y = 50
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "red")

y = -50
`P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
lines(x_seq, `P(X<=x|Y=y)`, col = "orange")


legend("bottomright", legend=c("y=0", "y=25","y=-25","y=50","y=-50"),
       col=c("green","blue", "purple","red","orange"), lty=1, cex=0.8)
  
}



