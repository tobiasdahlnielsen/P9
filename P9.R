## Load packages
source("Lib.R")


## Data selecting

spotspread <- data %>% transmute(Spread = DE-FR, SpreadForecast = DEForecast - FRForecast)
spotspread2 <- data %>% transmute(Spread = FR-DE, SpreadForecast = FRForecast - DEForecast) 
spotspredpseudo <- spotspread %>% pobs()
spotspredpseudo2 <- spotspread2 %>% pobs()

pseudoobsDE <- data %>% select(DE,DEForecast) %>% pobs()
pseudoobsFR <- data %>% select(FR,FRForecast) %>% pobs()

pseudoobsDEfit <- datafit %>% select(DE,DEForecast) %>% pobs()
pseudoobsFRfit <- datafit %>% select(FR,FRForecast) %>% pobs()

## Visualization of data
data %>% 
  ggplot(aes(x = StartUTC, y = DE, type = "scatter", color = "Obs")) + geom_line() + geom_line(aes(y = DEForecast, color = "Forcast"))

data %>% 
  ggplot(aes(x = StartUTC, y = FR, type = "scatter", color = "Obs")) + geom_line() + geom_line(aes(y = FRForecast, color = "Forcast"))

spotspread2 %>% 
  ggplot(aes(x = Spread, y = SpreadForecast, type = "scatter")) + geom_point() + xlim(-75,75) + ylim(-75,75)




## Non parametric estimation of Dependence measures

DEKendall <- data %>% select(DE,DEForecast) %>% cor(method = "kendall")
DESpearman <- data %>% select(DE,DEForecast) %>% cor(method = "spearman")
FRKendall <- data %>% select(FR,FRForecast) %>% cor(method = "kendall")
FRSpearman <- data %>% select(FR,FRForecast) %>% cor(method = "spearman")


lambda <- c(lower = fitLambda(pseudoobsDE)[2,1],
            upper = fitLambda(pseudoobsFR,lower.tail = FALSE)[2,1])


## Marginal distributions 



par(mfrow=c(1,2))
hist(data$DE)
hist(data$DEForecast)


par(mfrow=c(1,2))
hist(data$FR)
hist(data$FRForecast)




## Copula fitting

# select family of copula

DE_Copula_fit <- BiCopSelect(pobs(data$DE),pobs(data$DEForecast),familyset = NA)
FR_Copula_fit <- BiCopSelect(pobs(data$FR),pobs(data$FRForecast),familyset = NA)

DE_Copula_fit_ARMA <- BiCopSelect(pobs(datafit$DE),pobs(datafit$DEForecast),familyset = NA)
FR_Copula_fit_ARMA <- BiCopSelect(pobs(datafit$FR),pobs(datafit$FRForecast),familyset = NA)



# fitting with empirical margins

CopfitmplDE <- fitCopula(tCopula(), pseudoobsDE, method="mpl")
CopfitmlDE <- fitCopula(tCopula(), pseudoobsDE, method="ml")

CopfitmplDE_ARMA <- fitCopula(tCopula(), pseudoobsDEfit, method="mpl")
CopfitmlDE_ARMA <- fitCopula(tCopula(), pseudoobsDEfit, method="ml")

hist(cCopula(pseudoobsDEfit,CopfitmplDE_ARMA@copula)[,2])

hist(qnorm(as.numeric(cCopula(pseudoobsDEfit,CopfitmplDE_ARMA@copula)[,1]),mean = mean(datafit[,2]),sd=sd(datafit[,2])),breaks=100)

CopfitmplFR <- fitCopula(tCopula(), pseudoobsFR, method="mpl")
CopfitmlFR <- fitCopula(tCopula(), pseudoobsFR, method="ml")

CopfitmplFR_ARMA <- fitCopula(tCopula(), pseudoobsFR, method="mpl")
CopfitmlFR_ARMA <- fitCopula(tCopula(), pseudoobsFR, method="ml")


plot(CopfitmlDE@copula,n=10000)
plot(CopfitmplDE@copula,n=10000)
plot(CopfitmlDE_ARMA@copula,n=10000)
plot(CopfitmplDE_ARMA@copula,n=10000)



plot(CopfitmlFR@copula,n=10000)
plot(CopfitmplFR@copula,n=10000)
plot(CopfitmlFR_ARMA@copula,n=10000)
plot(CopfitmplFR_ARMA@copula,n=10000)


## parametric estimation of tail dependence

