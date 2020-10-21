## Load packages
source("Lib.R")


## Data selecting
data <- read.csv("C:/Users/sphj/Desktop/Matematik-Økonomi Studiet/Kandidaten/9. semester/P.9/spot_data.csv")
data <- data %>% mutate(StartUTC = as.POSIXct(StartUTC, tz = "UTC", format = "%Y-%m-%d %H:%M"))
spotspread <- data %>% transmute(Spread = DE-FR, SpreadForecast = DEForecast - FRForecast)


## Cleaning data (subsetting if needed)



## Visualization of data
data %>% 
  ggplot(aes(x = StartUTC, y = DE, type = "scatter", color = "Obs")) + geom_line() + geom_line(aes(y = DEForecast, color = "Forcast"))

data %>% 
  ggplot(aes(x = StartUTC, y = FR, type = "scatter", color = "Obs")) + geom_line() + geom_line(aes(y = FRForecast, color = "Forcast"))

spotspread %>% 
  ggplot(aes(x = Spread, y = SpreadForecast, type = "scatter")) + geom_point() + xlim(-75,75) + ylim(-75,75)



## Marginal distributions 


par(mfrow=c(1,2))
hist(data$DE)
hist(data$DEForecast)



par(mfrow=c(1,2))
hist(data$FR)
hist(data$FRForecast)

## Dependence measures

data %>% transmute(DE=DE, DEForecast=DEForecast) %>% cor(method = "kendall")
data %>% transmute(DE=DE, DEForecast=DEForecast) %>% cor(method = "spearman")
data %>% transmute(FR=FR, FRForecast=FRForecast) %>% cor(method = "kendall")
data %>% transmute(FR=FR, FRForecast=FRForecast) %>% cor(method = "spearman")



## Copula fitting


DE_Copula_fit <- BiCopSelect(pobs(data$DE),pobs(data$DEForecast),familyset = NA)


FR_Copula_fit <- BiCopSelect(pobs(data$FR),pobs(data$FRForecast),familyset = NA)



my_dist <- mvdc(tCopula(param = c(0.97,4.32), dim = 2), margins = c("gamma","gamma"), paramMargins = list(list(shape = x_shape, rate = x_rate), list(shape = y_shape, rate = y_rate)))

## Tail corr

