################################################################################
######################### Initial Data setup ###################################
################################################################################

source("Lib.R")

spot_data <- spot_data %>% mutate(dd = hour(StartUTC), wd = wday(StartUTC), yd = month(StartUTC),Yd = year(StartUTC))

acf(spot_data$DEForecast,300,main="Acf plot for Spot_DE_Forecast")




for (i in 1:4) {
  med <- median(spot_data[,i+1])
  mea <- mean(spot_data[,i+1])
  out.in <- which(spot_data[,i+1]>(med+10*mea))
  if (!(length(out.in)==0)) {
    spot_data[,(i+1)] <- medianswap(spot_data[,(i+1)],out.in)
  }
}

plot(spot_data$DE,type="l")
plot(spot_data$FR,type="l")
plot(spot_data$DEForecast,type="l")
plot(spot_data$FRForecast,type="l")

spot_data$StartUTC

Data = list()
Data[[1]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2016, month(StartUTC) < 7)
Data[[2]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2016, month(StartUTC) > 6)
Data[[3]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2017, month(StartUTC) < 7)
Data[[4]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2017, month(StartUTC) > 6)
Data[[5]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2018, month(StartUTC) < 7)
Data[[6]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2018, month(StartUTC) > 6)
Data[[7]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2019, month(StartUTC) < 7)
Data[[8]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2019, month(StartUTC) > 6)
Data[[9]] <- spot_data %>% filter(year(spot_data$StartUTC) == 2020)

FC_RMSE <- c(rep(0,9))
FC_MAE <- c(rep(0,9))
for (i in 1:9) {
FC_RMSE[i] <- sqrt( mean( (Data[[1]]$DE-Data[[i]]$DEForecast)^2 ))
FC_MAE[i] <- mean( abs(  Data[[i]]$DE - Data[[i]]$DEForecast  ) )
}

par(mfrow=c(3,3))
HY = c("2016h1","2016H2","2017H1","2017H2","2018H1","2018H2","2019H1","2019H2","2020")
for (i in 1:9) {
  plot(Data[[i]]$StartUTC, Data[[i]]$DE,type="l",ylab="Price",xlab=" ", main=HY[i])
}


Y16 <- spot_data %>% filter(year(spot_data$StartUTC) == 2016) %>%  select(StartUTC)
Y17 <- spot_data %>% filter(year(spot_data$StartUTC) == 2017) %>%  select(StartUTC)
Y18 <- spot_data %>% filter(year(spot_data$StartUTC) == 2018) %>%  select(StartUTC)
Y19 <- spot_data %>% filter(year(spot_data$StartUTC) == 2019) %>%  select(StartUTC)
Y20 <- spot_data %>% filter(year(spot_data$StartUTC) == 2019) %>%  select(StartUTC)

Y_length = c(length(Y16[,1]),length(Y16[,1]), length(Y17[,1]),length(Y17[,1]), length(Y18[,1]),length(Y18[,1]), 
             length(Y19[,1]), length(Y19[,1]),length(Y20[,1]),length(Y20[,1]))


acf(spot_data$DE,lag.max=300,main="")
acf(Data_Res$DE_res,lag.max=300,main="")

acf(spot_data$FR,lag.max=300,main="")
acf(Data_Res$FR_res,lag.max=300,main="")

acf(spot_data$DEForecast,lag.max=300,main="")
acf(Data_Res$DE_FC_res,lag.max=300,main="")

acf(spot_data$FRForecast,lag.max=300,main="")
acf(Data_Res$FR_FC_res,lag.max=300,main="")

ggseasonplot(ts(spot_data$DE[1:(24*10)],frequency=24), year.labels = FALSE,year.labels.left = F)

################################################################################
#################### Fitting a model to DE _ Actual ############################
################################################################################

models_DE = list()
for (i in 1:9) {
models_DE[[i]] <- lm(Data[[i]]$DE ~
              time(Data[[i]][,1])+
              # I(time(Data[[i]][,1])^2)+
              #I(time(Data[[i]][,1])^3)+
              cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
              sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
              #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
              cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
              sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
              cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
              sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
              #factor(Data[[i]]$Yd)+
              #factor(Data[[i]]$yd)+
              factor(Data[[i]]$wd)+
              factor(Data[[i]]$dd)
)
}


Q = list()
orders <- list()
for (i in 1:9) {
Q[[i]] <- auto.arima(models_DE[[i]]$residuals)
orders[[i]] <- Q[[i]]$coef
}

od = list(c(2,0,2),c(5,0,2),c(1,0,2),c(4,0,1),c(5,0,3),c(4,0,1),c(2,0,3),c(1,0,2),c(4,0,2))

par(mfrow=c(3,3))
sarma24_DE = list()
for (i in 1:9) {
  sarma24_DE[[i]] <- arima(models_DE[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  acf(sarma24_DE[[i]]$residuals,lag.max=400)
}

sarma24_DE_aic_1 = list()
#sarma24_DE_aic_2 = list()
for (i in 1:9) {
  acf(sarma24_DE[[i]]$residuals,lag.max=40)
  #pacf(sarma24_DE[[i]]$residuals,lag.max=400)
  sarma24_DE_aic_1[[i]] <- sarma24_DE[[i]]$aic
  #sarma24_DE_aic_2[[i]] <- sarma24_DE[[i]]$aic
  #plot(sarma24_DE[[i]]$residuals,type="l")
}



sarma168_DE = list() 
for (i in 1:9) {
    sarma168_DE[[i]] <- arima(sarma24_DE[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
    acf(sarma168_DE[[i]]$residuals,lag.max=400)
}


aic_168_DE_1 = list()
for (i in 1:9) {
  Acf(sarma168_DE[[i]]$residuals,lag.max=400)
  #pacf(sarma168_DE[[i]]$residuals,lag.max=400)
  aic_168_DE_1[[i]] = sarma168_DE[[i]]$aic
  #aic_168_DE_2[[i]] = sarma168_DE[[i]]$aic
  #plot(sarma168_DE[[i]]$residuals)
}

arma_DE = list()
for (i in 1:9) {
  arma_DE[[i]] <- auto.arima(sarma168_DE[[i]]$residuals)
  Acf(arma_DE[[i]]$residuals)
}

for (i in 1:9) {
  Acf(arma_DE[[i]]$residuals,lag.max=300)
}

Residuals_DE = list()
for (i in 1:9) {
  Residuals_DE[[i]] <- arma_DE[[i]]$residuals
  plot(Residuals_DE[[i]],type="l")
}


data_DE <- unlist(Residuals_DE)
data_FR <- unlist(Residuals_FR)
data_DE_FC <- unlist(Residuals_DE_FC)
data_FR_FC <- unlist(Residuals_FR_FC)


Acf(data_DE_FC,lag.max=300,main="",ylim=c(-0.1,0.3))
plot(spot_data$StartUTC,data_DE_FC,type="l",xlab="Time",ylab="DE_FC_spot_residuals")
hist(data_DE_FC,breaks=160,xlim=c(-40,40),main="",xlab="DE_FC_spot_residuals")


################################################################################
#################### Fitting a model to FR _ Actual ############################
################################################################################

plot(spot_data$FR)

models_FR = list()
for (i in 1:9) {
  models_FR[[i]] <- lm(Data[[i]]$FR ~
                      time(Data[[i]][,1])+
                      # I(time(Data[[i]][,1])^2)+
                      #I(time(Data[[i]][,1])^3)+
                      cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
                      sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                      #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                      sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                      cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                      sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                      #factor(Data[[i]]$Yd)+
                      #factor(Data[[i]]$yd)+
                      factor(Data[[i]]$wd)+
                      factor(Data[[i]]$dd)
  )
}

par(mfrow=c(3,3))
for (i in 1:9) {
  acf(models_FR[[i]]$residuals,75)
}


Q = list()
orders <- list()
for (i in 1:9) {
  Q[[i]] <- auto.arima(models_FR[[i]]$residuals)
  orders[[i]] <- Q[[i]]$coef
}

od = list(c(4,0,0),c(2,0,1),c(4,0,5),c(4,0,1),c(5,0,4),c(1,0,1),c(3,0,5),c(2,0,3),c(5,0,3))

sarma24_FR = list()
for (i in 1:9) {
  sarma24_FR[[i]] <- arima(models_FR[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  acf(sarma24_FR[[i]]$residuals,lag.max=100)
}

sarma24_FR_aic_1 = list()
#sarma24_FR_aic_2 = list()
for (i in 1:9) {
  Acf(sarma24_FR[[i]]$residuals,lag.max=400)
  #pacf(sarma24_FR[[i]]$residuals,lag.max=400)
  #sarma24_FR_aic_1[[i]] <- sarma24_FR[[i]]$aic
  #sarma24_FR_aic_2[[i]] <- sarma24_FR[[i]]$aic
}


sarma168_FR = list() 
for (i in 1:9) {
  sarma168_FR[[i]] <- arima(sarma24_FR[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
  acf(sarma168_FR[[i]]$residuals,lag.max=400)
}


aic168_FR_1 = list()
#aic168_FR_2 = list()
for (i in 1:9) {
  #Acf(sarma168_FR[[i]]$residuals,lag.max=40)
  #pacf(sarma168_FR[[i]]$residuals,lag.max=400)
  #aic168_FR_1[[i]] = sarma168_FR[[i]]$aic
  #aic168_FR_2[[i]] = sarma168_FR[[i]]$aic
  plot(sarma168_FR[[i]]$residuals)
}

arma_FR = list()
for (i in 1:9) {
  arma_FR[[i]] <- auto.arima(sarma168_FR[[i]]$residuals)
  Acf(arma_FR[[i]]$residuals)
}

for (i in 1:9) {
  Acf(arma[[i]]$residuals,lag.max=300)
}

Residuals_FR = list()
for (i in 1:9) {
  Residuals_FR[[i]] <- arma_FR[[i]]$residuals
  plot(Residuals_FR[[i]],type="l")
}




################################################################################
#################### Fitting a model to FR _ Forecast ##########################
################################################################################

models_FR_FC = list()
for (i in 1:9) {
  models_FR_FC[[i]] <- lm(Data[[i]]$FRForecast ~
                         time(Data[[i]][,1])+
                         # I(time(Data[[i]][,1])^2)+
                         #I(time(Data[[i]][,1])^3)+
                         cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
                         sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                         #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                         cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                         sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                         cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                         sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                         #factor(Data[[i]]$Yd)+
                         #factor(Data[[i]]$yd)+
                         factor(Data[[i]]$wd)+
                         factor(Data[[i]]$dd)
  )
}

for (i in 1:9) {
  acf(models_FR_FC[[i]]$residuals,75)
}


Q = list()
orders <- list()
for (i in 1:9) {
  Q[[i]] <- auto.arima(models_FR_FC[[i]]$residuals)
  orders[[i]] <- Q[[i]]$coef
}

od = list(c(3,0,2),c(1,0,3),c(5,0,0),c(4,0,1),c(4,0,1),c(4,0,1),c(3,0,2),c(3,0,1),c(1,0,1))

sarma24_FR_FC = list()
for (i in 1:9) {
  sarma24_FR_FC[[i]] <- arima(models_FR_FC[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  acf(sarma24_FR_FC[[i]]$residuals,lag.max=100)
}

sarma24_FR_FC_aic_1 = list()
#sarma24_FR_aic_2 = list()
for (i in 1:9) {
  Acf(sarma24_FR_FC[[i]]$residuals,lag.max=50)
  #pacf(sarma24_FR_FC[[i]]$residuals,lag.max=400)
  sarma24_FR_FC_aic_1[[i]] <- sarma24_FR_FC[[i]]$aic
  #sarma24_FR_FC_aic_2[[i]] <- sarma24_FR_FC[[i]]$aic
}


sarma168_FR_FC = list() 
for (i in 1:9) {
  sarma168_FR_FC[[i]] <- arima(sarma24_FR_FC[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
  acf(sarma168_FR_FC[[i]]$residuals,lag.max=400)
}


aic168_FR_FC_1 = list()
#aic168_FR_FC_2 = list()
sarma168_FR_FC_res <- c()
for (i in 1:9) {
  #Acf(sarma168_FR_FC[[i]]$residuals,lag.max=40)
  #pacf(sarma168_FR_FC[[i]]$residuals,lag.max=400)
  #aic168_FR_FC_1[[i]] = sarma168_FR_FC[[i]]$aic
  #aic168_FR_FC_2[[i]] = sarma168_FR_FC[[i]]$aic
  #plot(sarma168_FR_FC[[i]]$residuals)
  sarma168_FR_FC_res <- c(sarma168_FR_FC_res,sarma168_FR_FC[[i]]$residuals)
}

hist(sarma168_FR_FC_res,breaks=400,prob=TRUE,xlim=c(-40,40),ylim=c(0,0.2))
lines(density(sarma168_FR_FC_res))

arma_FR_FC = list()
for (i in 1:9) {
  arma_FR_FC[[i]] <- auto.arima(sarma168_FR_FC[[i]]$residuals)
  #Acf(arma_FR[[i]]$residuals)
  print(i)
}

for (i in 1:9) {
  Acf(arma_FR_FC[[i]]$residuals,lag.max=300)
}

Residuals_FR_FC = list()
for (i in 1:9) {
  Residuals_FR_FC[[i]] <- arma_FR_FC[[i]]$residuals
  plot(Residuals_FR_FC[[i]],type="l")
}

data_FR_FC <- unlist(Residuals_FR_FC)

par(mfrow=c(3,3))
test_FR_FC_res <- c()
for (i in 1:9) {
  #hist(arma_FR_FC[[i]]$residuals,prob=T,breaks=200)
  #lines(density(arma_FR_FC[[i]]$residuals),col="red")  
  test_FR_FC_res <- c(test_FR_FC_res, arma_FR_FC[[i]]$residuals)
}
par(mfrow=c(1,1))
hist(test_FR_FC_res,prob=T,breaks=400,xlim=c(-40,40),ylim=c(0,0.3))
lines(density(test_FR_FC_res))

ggplot() +
  geom_histogram( aes(x = test_FR_FC_res, y = ..density..), bins = 200) +
  geom_density( aes( x = test_FR_FC_res, color = 'Residuals')) +
  geom_density( aes( x = dsstd(seq(-50,50,by=0.1), mean = marg_DE_sstd$estimate[1], sd = marg_DE_sstd$estimate[2], nu =marg_DE_sstd$estimate[3], xi=marg_DE_sstd$estimate[4]), color = "SSTD")) +
  xlim(c(-40,40)) +
  ylim(c(0,0.3))


  
################################################################################
#################### Fitting a model to DE _ Forecast ##########################
################################################################################

models_DE_FC = list()
for (i in 1:9) {
  models_DE_FC[[i]] <- lm(Data[[i]]$DEForecast ~
                            time(Data[[i]][,1])+
                            # I(time(Data[[i]][,1])^2)+
                            #I(time(Data[[i]][,1])^3)+
                            cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
                            sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            #factor(Data[[i]]$Yd)+
                            #factor(Data[[i]]$yd)+
                            factor(Data[[i]]$wd)+
                            factor(Data[[i]]$dd)
  )
}

for (i in 1:9) {
  acf(models_DE_FC[[i]]$residuals,75)
}


Q = list()
orders <- list()
for (i in 1:9) {
  Q[[i]] <- auto.arima(models_DE_FC[[i]]$residuals)
  orders[[i]] <- Q[[i]]$coef
}

od = list(c(2,0,5),c(4,0,1),c(1,0,5),c(1,0,2),c(1,0,2),c(1,0,2),c(1,0,2),c(2,0,4),c(1,0,3))

sarma24_DE_FC = list()
for (i in 1:9) {
  sarma24_DE_FC[[i]] <- arima(models_DE_FC[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  acf(sarma24_DE_FC[[i]]$residuals,lag.max=100)
}

sarma24_DE_FC_aic_1 = list()
#sarma24_DE_aic_2 = list()
for (i in 1:9) {
  Acf(sarma24_DE_FC[[i]]$residuals,lag.max=50)
  #pacf(sarma24_DE_FC[[i]]$residuals,lag.max=400)
  sarma24_DE_FC_aic_1[[i]] <- sarma24_DE_FC[[i]]$aic
  #sarma24_DE_FC_aic_2[[i]] <- sarma24_DE_FC[[i]]$aic
}


sarma168_DE_FC = list() 
for (i in 1:9) {
  sarma168_DE_FC[[i]] <- arima(sarma24_DE_FC[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
  acf(sarma168_DE_FC[[i]]$residuals,lag.max=400)
}


aic168_DE_FC_1 = list()
#aic168_DE_FC_2 = list()
for (i in 1:9) {
  Acf(sarma168_DE_FC[[i]]$residuals,lag.max=40)
  #pacf(sarma168_DE_FC[[i]]$residuals,lag.max=400)
  aic168_DE_FC_1[[i]] = sarma168_DE_FC[[i]]$aic
  #aic168_DE_FC_2[[i]] = sarma168_DE_FC[[i]]$aic
  #plot(sarma168_DE_FC[[i]]$residuals)
}

arma_DE_FC = list()
for (i in 1:9) {
  arma_DE_FC[[i]] <- auto.arima(sarma168_DE_FC[[i]]$residuals)
  Acf(arma_DE_FC[[i]]$residuals)
}

for (i in 1:9) {
  Acf(arma_DE_FC[[i]]$residuals,lag.max=300)
}

Residuals_FR_FC = list()
for (i in 1:9) {
  Residuals_FR_FC[[i]] <- arma_FR_FC[[i]]$residuals
}


for (i in 1:9) {
  plot(Residuals_DE_FC[[i]],type="l")
}




################################################################################
########################### Combine Data #######################################
################################################################################

DE_res <- unlist(Residuals_DE)
FR_res <- unlist(Residuals_FR)
DE_FC_res <- unlist(Residuals_DE_FC)
FR_FC_res <- unlist(Residuals_FR_FC)

Data_Res <- data.frame(StartUTC=spot_data$StartUTC,DE_res,FR_res,DE_FC_res,FR_FC_res)
View(Data_Res)


c(sqrt(mean(Data_Res$DE_res^2)),sqrt(mean(Data_Res$FR_res^2)),sqrt(mean(Data_Res$DE_FC_res^2)),sqrt(mean(Data_Res$FR_FC_res^2)))

par(mfrow=c(2,2))
plot(Data_Res$StartUTC,DE_res,type="l",xlab="Time",ylab="Price",main="DE_Spot")
plot(Data_Res$StartUTC,FR_res,type="l",xlab="Time",ylab="Price",main="FR_Spot")
plot(Data_Res$StartUTC,DE_FC_res,type="l",xlab="Time",ylab="Price",main="DE_Forecast")
plot(Data_Res$StartUTC,FR_FC_res,type="l",xlab="Time",ylab="Price",main="FR_Forecast")

Acf(DE_res,main="DE_Spot")
Acf(FR_res,main="FR_Spot")
Acf(DE_FC_res,main="DE_Forecast")
Acf(FR_FC_res,main="FR_Forecast")

hist(Data_Res$DE_res,breaks=80,xlim=c(-40,40),main="DE_spot",xlab="Price")
hist(Data_Res$FR_res,breaks=80,xlim=c(-40,40),main="FR_Spot",xlab="Price")
hist(Data_Res$DE_FC_res,breaks=80,xlim=c(-40,40),main="DE_Forecast",xlab="Price")
hist(Data_Res$FR_FC_res,breaks=80,xlim=c(-40,40),main="FR_Forecast",xlab="Price")


LB_test_DE = c()
LB_test_FR = c()
LB_test_DE_FC = c()
LB_test_FR_FC = c()
for (i in 1:5) {
test1 = Box.test(Data_Res$DE_res,type="Ljung-Box",lag=i)
test2 = Box.test(Data_Res$FR_res,type="Ljung-Box",lag=i)
test3 = Box.test(Data_Res$DE_FC_res,type="Ljung-Box",lag=i)
test4 = Box.test(Data_Res$FR_FC_res,type="Ljung-Box",lag=i)
LB_test_DE = c(LB_test_DE,test1$p.value)
LB_test_FR = c(LB_test_FR,test2$p.value)
LB_test_DE_FC = c(LB_test_DE_FC,test3$p.value)
LB_test_FR_FC = c(LB_test_FR_FC,test4$p.value)
}
LB_test_DE
LB_test_FR
LB_test_DE_FC
LB_test_FR_FC

adf_DE    = adf.test(Data_Res$DE_res)
adf_FR    = adf.test(Data_Res$FR_res)
adf_DE_FC = adf.test(Data_Res$DE_FC_res)
adf_FR_FC = adf.test(Data_Res$FR_FC_res)
list(adf_DE,adf_FR,adf_DE_FC,adf_FR_FC)



################################################################################
######################### Estimating Marginals ################################
################################################################################

DE_marg <- ecdf(Data_Res$DE_res)
FR_marg <- ecdf(Data_Res$FR_res)
DE_FC_marg <- ecdf(Data_Res$DE_FC_res)
FR_FC_marg <- ecdf(Data_Res$FR_FC_res)

plot(DE_marg,xlim=c(-20,20))
plot(FR_marg,xlim=c(-20,20))
plot(DE_FC_marg,xlim=c(-20,20))
plot(FR_FC_marg,xlim=c(-20,20))

par(mfrow=c(1,1))
plot(DE_marg,xlim=c(-20,20))
lines(FR_marg)
lines(DE_FC_marg)
lines(FR_FC_marg)
par(mfrow=c(2,2))


DE_pdf <- estimatePDF(Data_Res$DE_res)
plot(DE_pdf$x,DE_pdf$pdf)


################################################################################
############# Fitting Copulas and computing Dependence Measures ################
################################################################################

DE_Copula_select <- BiCopSelect(pobs(Data_Res$DE_FC_res),pobs(Data_Res$DE_res),familyset = NA)
FR_Copula_select <- BiCopSelect(pobs(Data_Res$FR_FC_res),pobs(Data_Res$FR_res),familyset = NA)


fam <- c(1,2,3,4)
DE_emp_cops <- list()
FR_emp_cops <- list()
AICs_DE <- c()
AICs_FR <- c()
for (i in 1:4) {
  DE_emp_cops[[i]] <- BiCopSelect(pobs(Data_Res$DE_FC_res),pobs(Data_Res$DE_res),familyset = fam[i])
  FR_emp_cops[[i]] <- BiCopSelect(pobs(Data_Res$FR_FC_res),pobs(Data_Res$FR_res),familyset = fam[i])
  AICs_DE[i] <- DE_emp_cops[[i]]$AIC
  AICs_FR[i] <- FR_emp_cops[[i]]$AIC
  print(i)
}


summary(DE_Copula_select)

plot(DE_Copula_select,xlab="DE_FC",ylab="DE")
plot(FR_Copula_select,xlab="FR_FC",ylab="FR")

pseudoobsDE <- Data_Res %>% dplyr::select(DE_res,DE_FC_res) %>% pobs()
pseudoobsFR <- Data_Res %>% dplyr::select(FR_res,FR_FC_res) %>% pobs()



# ### Fitting one with copula fit ### 
#
# 
# Copula_DE <- fitCopula(tCopula(), pseudoobsDE, method="mpl")
# Copula_FR <- fitCopula(tCopula(), pseudoobsFR, method="mpl")
# summary(Copula_DE)
# summary(Copula_FR)


### Calculating Dependence Measures ### 

DE_Kendall  <- Data_Res %>% dplyr::select(DE_res,DE_FC_res) %>% cor(method = "kendall")
DE_Spearman <- Data_Res %>% dplyr::select(DE_res,DE_FC_res) %>% cor(method = "spearman")
FR_Kendall  <- Data_Res %>% dplyr::select(FR_res,FR_FC_res) %>% cor(method = "kendall")
FR_Spearman <- Data_Res %>% dplyr::select(FR_res,FR_FC_res) %>% cor(method = "spearman")

lambda_DE <- c(lower = fitLambda(pseudoobsDE)[2,1],
            upper = fitLambda(pseudoobsDE,lower.tail = FALSE)[2,1])

lambda_FR <- c(lower = fitLambda(pseudoobsFR)[2,1],
               upper = fitLambda(pseudoobsFR,lower.tail = FALSE)[2,1])

#lambda_DE_test <- c(lower = fitLambda(cbind( pobs(spot_data$DE),pobs(spot_data$DEForecast) ))[2,1],
#               upper = fitLambda(cbind( pobs(spot_data$DE),pobs(spot_data$DEForecast) ),lower.tail = FALSE)[2,1])

### Parametrisk taildependence ###
DE_Copula_select$taildep
FR_Copula_select$taildep
DE_FR_Copula$taildep

pairs.copuladata(Copula_DE@copula)

Sim_DE <- BiCopSim(20000,family=2,0.4015,2.9490)
plot(Sim_DE)
contour(Sim_DE)

################################################################################
####################### Creating model reversion ###############################
################################################################################
Fitted_DE <- list()
Fitted_FR <- list()
Fitted_DE_FC <- list()
Fitted_FR_FC <- list()
Residuals_DE <- list()
Residuals_FR <- list()
Residuals_DE_FC <- list()
Residuals_FR_FC <- list()

for (i in 1:9) {
  Fitted_DE[[i]] <- arma_DE[[i]]$fitted + fitted(sarma168_DE[[i]]) + fitted(sarma24_DE[[i]]) + models_DE[[i]]$fitted.values
  Fitted_FR[[i]] <- arma_FR[[i]]$fitted + fitted(sarma168_FR[[i]]) + fitted(sarma24_FR[[i]]) + models_FR[[i]]$fitted.values
  Fitted_DE_FC[[i]] <- arma_DE_FC[[i]]$fitted + fitted(sarma168_DE_FC[[i]]) + fitted(sarma24_DE_FC[[i]]) + models_DE_FC[[i]]$fitted.values
  Fitted_FR_FC[[i]] <- arma_FR_FC[[i]]$fitted + fitted(sarma168_FR_FC[[i]]) + fitted(sarma24_FR_FC[[i]]) + models_FR_FC[[i]]$fitted.values
  Residuals_DE[[i]] <- arma_DE[[i]]$residuals + sarma168_DE[[i]]$residuals + sarma24_DE[[i]]$residuals + models_DE[[i]]$residuals
  Residuals_FR[[i]] <- arma_FR[[i]]$residuals + sarma168_FR[[i]]$residuals + sarma24_FR[[i]]$residuals + models_FR[[i]]$residuals
  Residuals_DE_FC[[i]] <- arma_DE_FC[[i]]$residuals + sarma168_DE_FC[[i]]$residuals + sarma24_DE_FC[[i]]$residuals + models_DE_FC[[i]]$residuals
  Residuals_FR_FC[[i]] <- arma_FR_FC[[i]]$residuals + sarma168_FR_FC[[i]]$residuals + sarma24_FR_FC[[i]]$residuals + models_FR_FC[[i]]$residuals
}
fitted_DE <- unlist(Fitted_DE)
fitted_FR <- unlist(Fitted_FR)
fitted_DE_FC <- unlist(Fitted_DE_FC)
fitted_FR_FC <- unlist(Fitted_FR_FC)
residuals_DE <- unlist(Residuals_DE)
residuals_FR <- unlist(Residuals_FR)
residuals_DE_FC <- unlist(Residuals_DE_FC)
residuals_FR_FC <- unlist(Residuals_FR_FC)

DE_sim_prices <- DE_simulation_res + fitted_DE
FR_sim_prices <- FR_simulation_res + fitted_FR
DE_FC_sim_prices <- DE_FC_simulation_res + fitted_DE_FC
FR_FC_sim_prices <- FR_FC_simulation_res + fitted_FR_FC



################################################################################
################# Computing Conditional Distributions DE #######################
################################################################################

n_simulations <- 100
DE <- list()
CDFs_DE <- list()
Percentile = c(0.00001,0.0001,0.0005, 0.025, 0.5, 0.975, 0.9995,0.9999,0.99999)
for (j in 1:length(Percentile)) {
  #Forecasted_value_DE[j] <- quantile(spot_data$DEForecast,probs=Percentile[j])
  
  cond_sim_cop_DE <- matrix(nrow=100000,ncol=n_simulations)
  for (i in 1:n_simulations) {
    cond_sim <- BiCopCondSim(100000, cond.val = Percentile[j], cond.var = 2, DE_Copula_select)
    cond_sim_cop_DE[,i] <- quantile(Data_Res$DE_res,probs=cond_sim) 
    #print(i)
  }
  Forecasted_value_DE[j] <- quantile(Data_Res$DE_FC_res,probs=Percentile[j]) 
  cond_sim_cop <- cbind(cond_sim_cop_DE,Forecasted_value_DE[j])
  ### If we want to reverse back from residuals to prices time series ###
  #cond_sim_cop[,1:n_simulations] <- cond_sim_cop[,1:n_simulations] + fitted_DE
  #cond_sim_cop[,n_simulations+1] <- cond_sim_cop[,n_simulations+1] + fitted_DE_FC
  
  DE[[j]] <- cbind(rowMeans(cond_sim_cop[,1:n_simulations]),cond_sim_cop[,n_simulations+1])
  CDFs_DE[[j]] <- ecdf(DE[[j]][,1])  
  print(j)
}
names(Forecasted_value_DE) <- names(Forecasted_value)


plot(CDFs_DE[[1]],xlim=c(-60,80),xlab="x",ylab="P( DE_res < x | DE_FC_res = y )",main="",col=1)
for (j in 2:length(Percentile)) {
  lines(CDFs_DE[[j]],col=j)
}
spaces <- c("  ","    ","    ","      ","       ","    ","  ","  ","")
temp <- legend("bottomright",legend=c("","","","","","","","",""),lwd=2,col=cols
               ,text.width = strwidth("1,000,000,000,000,000"), xjust = 1, yjust = 1)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c(paste(round(Forecasted_value_DE,digits = 2),paste0(" = Q_",names(Forecasted_value_DE)),spaces)), pos = 2)



L <- c()
U <- c()
means <- c()
for (j in 1:length(Percentile)) {
  sim1 <- BiCopCondSim(10000, cond.val = Percentile[j], cond.var = 2, DE_Copula_select)
  cond_pdf_DE <- quantile(Data_Res$DE_res,probs=sim1)  
  
  L[j] <- mean(cond_pdf_DE) - quantile(cond_pdf_DE, 1 - 0.05 / 2) * sd(cond_pdf_DE) / sqrt(length(cond_pdf_DE))
  U[j] <- mean(cond_pdf_DE) + quantile(cond_pdf_DE, 1 - 0.05 / 2) * sd(cond_pdf_DE) / sqrt(length(cond_pdf_DE))
  
  means[j] <- mean(cond_pdf_DE)
}

plot(means,type="l",ylim=c(-62,62),axes=FALSE, lwd = 2,
     ylab="E[ DE_res | DE_FC_res ]",xlab="Quantiles for DE_Forecast_residuals")
lines(Forecasted_value_DE,col="purple",lwd=2,lty=2)
lines(L,col="red",lwd=2)
lines(U,col="blue",lwd=2)
axis(2,ylim=c(0,2))
axis(1, at=1:9, labels=paste0("Q_",names(Forecasted_value_DE)))
legend("bottomright",legend=c("E[ DE_res | DE_FC_res ]", "Upper Confidence",
                              "Lower Conficence", "DE_FC_res")
                    ,lty=c(1,1,1,2), lwd=2,col=c("black","blue","red","purple"))



################################################################################
################# Computing Conditional Distributions FR #######################
################################################################################


n_simulations <- 100
FR <- list()
CDFs_FR <- list()
Percentile = c(0.00001,0.0001,0.0005, 0.025, 0.5, 0.975, 0.9995,0.9999,0.99999)
for (j in 1:length(Percentile)) {
  #Forecasted_value_FR[j] <- quantile(spot_data$FRForecast,probs=Percentile[j])
  
  cond_sim_cop_FR <- matrix(nrow=100000,ncol=n_simulations)
  for (i in 1:n_simulations) {
    cond_sim <- BiCopCondSim(100000, cond.val = Percentile[j], cond.var = 2, FR_Copula_select)
    cond_sim_cop_FR[,i] <- quantile(Data_Res$FR_res,probs=cond_sim) 
    #print(i)
  }
  Forecasted_value_FR[j] <- quantile(Data_Res$FR_FC_res,probs=Percentile[j]) 
  cond_sim_cop <- cbind(cond_sim_cop_FR,Forecasted_value_FR[j])
  ### If we want to reverse back from residuals to prices time series ###
  #cond_sim_cop[,1:n_simulations] <- cond_sim_cop[,1:n_simulations] + fitted_FR
  #cond_sim_cop[,n_simulations+1] <- cond_sim_cop[,n_simulations+1] + fitted_FR_FC
  
  FR[[j]] <- cbind(rowMeans(cond_sim_cop[,1:n_simulations]),cond_sim_cop[,n_simulations+1])
  CDFs_FR[[j]] <- ecdf(FR[[j]][,1])  
  print(j)
}
names(Forecasted_value_FR) <- names(Forecasted_value)


plot(CDFs_FR[[1]],xlim=c(-60,180),xlab="x",ylab="P( FR_res < x | FR_FC_res = y )",main="",col=1)
for (j in 2:length(Percentile)) {
  lines(CDFs_FR[[j]],col=j)
}
spaces <- c("  ","    ","    ","     ","       ","    ","  ","  ","")
temp <- legend("bottomright",legend=c("","","","","","","","",""),lwd=2,col=cols
               ,text.width = strwidth("1,000,000,000,000,000"), xjust = 1, yjust = 1)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c(paste(round(Forecasted_value_FR,digits = 2),paste0(" = Q_",names(Forecasted_value)),spaces)), pos = 2)



L <- c()
U <- c()
means <- c()
for (j in 1:length(Percentile)) {
  sim1 <- BiCopCondSim(100000, cond.val = Percentile[j], cond.var = 2, FR_Copula_select)
  cond_pdf_FR <- quantile(Data_Res$FR_res,probs=sim1)  

  L[j] <- mean(cond_pdf_FR) - quantile(cond_pdf_FR, 1 - 0.05 / 2) * sd(cond_pdf_FR) / sqrt(length(cond_pdf_FR))
  U[j] <- mean(cond_pdf_FR) + quantile(cond_pdf_FR, 1 - 0.05 / 2) * sd(cond_pdf_FR) / sqrt(length(cond_pdf_FR))
  
  means[j] <- mean(cond_pdf_FR)
  print(j)
}

plot(means,type="l",ylim=c(-200,250),axes=FALSE, lwd = 2,
     ylab="E[ FR_res | FR_FC_res ]",xlab="Quantiles for FR_Forecast_residuals")
lines(Forecasted_value_FR,col="purple",lwd=2,lty=2)
lines(L,col="red",lwd=2)
lines(U,col="blue",lwd=2)
axis(2,ylim=c(0,2))
axis(1, at=1:9, labels=paste0("Q_",names(Forecasted_value)))
legend("bottomright",legend=c("E[ FR_res | FR_FC_res ]", "Upper Confidence",
                              "Lower Conficence", "FR_FC_res")
       ,lty=c(1,1,1,2), lwd=2,col=c("black","blue","red","purple"))



################################################################################
################ Computing dependence measures over time #######################
################################################################################

### fitting copulas on half years ### 
copulas_DE <- list()
copulas_FR <- list()
taildependence_DE <- list()
taildependence_FR <- list()
NP_TD_DE_L <- list()
NP_TD_FR_L <- list()
NP_TD_DE_U <- list()
NP_TD_FR_U <- list()
rho_DE <- list()
tau_DE <- list()
rho_FR <- list()
tau_FR <- list()

for (i in 1:9) {
copulas_DE[[i]] <- BiCopSelect(pobs(Residuals_DE[[i]]),pobs(Residuals_DE_FC[[i]]),familyset = NA)
copulas_FR[[i]] <- BiCopSelect(pobs(Residuals_FR[[i]]),pobs(Residuals_FR_FC[[i]]),familyset = NA)    
taildependence_DE[[i]] <- copulas_DE[[i]]$taildep[1]
taildependence_FR[[i]] <- copulas_FR[[i]]$taildep[1]
NP_TD_DE_L[[i]] <- c(lower = fitLambda(cbind(pobs(Residuals_DE[[i]]),pobs(Residuals_DE_FC[[i]])))[2,1])
NP_TD_FR_L[[i]] <- c(lower = fitLambda(cbind(pobs(Residuals_FR[[i]]),pobs(Residuals_FR_FC[[i]])))[2,1])
NP_TD_DE_U[[i]] <- c(upper = fitLambda(cbind(pobs(Residuals_DE[[i]]),pobs(Residuals_DE_FC[[i]])),lower.tail = FALSE)[2,1])
NP_TD_FR_U[[i]] <- c(upper = fitLambda(cbind(pobs(Residuals_FR[[i]]),pobs(Residuals_FR_FC[[i]])),lower.tail = FALSE)[2,1])
rho_DE[[i]] <- cor(Residuals_DE[[i]],Residuals_DE_FC[[i]],method = "spearman") 
tau_DE[[i]] <- cor(Residuals_DE[[i]],Residuals_DE_FC[[i]],method = "kendall")
rho_FR[[i]] <- cor(Residuals_FR[[i]],Residuals_FR_FC[[i]],method = "spearman")
tau_FR[[i]] <- cor(Residuals_FR[[i]],Residuals_FR_FC[[i]],method = "kendall")
print(i)
}




HY <- c("2016_1","2016_2","2017_1","2017_2","2018_1","2018_2","2019_1","2019_2","2020_1")
HHY <- data.frame(cbind(rho_DE=unlist(rho_DE),rho_FR=unlist(rho_FR),
                        tau_DE=unlist(tau_DE),tau_FR=unlist(tau_FR),
                        TD_DE = unlist(taildependence_DE),TD_FR = unlist(taildependence_FR)),
                        NP_TD_DE_L = unlist(NP_TD_DE_L), NP_TD_DE_U = unlist(NP_TD_DE_U), 
                        NP_TD_FR_L = unlist(NP_TD_FR_L), NP_TD_FR_U = unlist(NP_TD_FR_U),row.names = HY)

par(mfrow=c(1,3))
plot(HHY$rho_DE,type="l",axes=FALSE,xlab="",ylab="Spearman's Rho",ylim=c(0.3,0.8))  
lines(HHY$rho_FR,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")

plot(HHY$tau_DE,type="l",axes=FALSE,xlab="",ylab="Kendall's Tau",ylim=c(0.3,0.8))  
lines(HHY$tau_FR,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")
#legend("topright",legend = c("Germany","France"), col=c("black","blue"),lty=1, cex=1)

plot(HHY$TD_DE,type="l",axes=FALSE,xlab="",ylab="Tail dependence",ylim=c(0.2,0.7))  
lines(HHY$TD_FR,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")
legend("topright",legend = c("Germany","France"), col=c("black","blue"),lty=1, cex=1.2)

par(mfrow=c(1,2))
plot(HHY$NP_TD_DE_L,type="l",axes=FALSE,ylim=c(0.2,0.7),xlab="",ylab="Non-parametric Tail dependence",main="Germany")
lines(HHY$NP_TD_DE_U,col="red")
#lines(HHY$TD_DE,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")
plot(HHY$NP_TD_FR_L,type="l",axes=FALSE,ylim=c(0.20,0.7),xlab="",ylab="",main="France")
lines(HHY$NP_TD_FR_U,col="red")
#lines(HHY$TD_FR,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")
legend("topright",legend = c("Lower","Upper","parametric"), col=c("black","red","blue"),lty=1, cex=0.9)


par(mfrow=c(1,1))
plot(HHY$TD_DE,type="l",axes=FALSE,ylim=c(0.10,0.5),xlab="",ylab="Parametric Tail dependence",main="")
lines(HHY$TD_FR,col="blue")
axis(2,ylim=c(0,1))
axis(1, at=1:9, labels=HY,xlab="")
legend("topright",legend = c("Germany","France"), col=c("black","blue"),lty=1, cex=1.4)



dseq <- c(2,4,3,5)
par(mfrow=c(1,2))
lim <- list(c(0,0.25),c(0,0.25),c(0,0.25),c(0,0.25))
b <- c(100,100,450,450)
for (i in 1:4){
  hist(Data_Res[,dseq[i]], prob=T, col="skyblue2",breaks=b[i],
       main=paste("Density of",names(Data_Res)[dseq[i]]),xlab="",xlim=c(-30,30),ylim=lim[[i]])
  lines(density(Data_Res[,dseq[i]], adjust=2),type="l", col="red", lwd=2) 
  curve(dnorm(x, 0, sd(Data_Res[,dseq[i]])), add=T, lty="dotted")
  legend("topright", legend=c("Empirical","Normal"),
         col=c("red","black"), lty=c(1,2), cex=0.8)
}
  

RMSE_9 <- c(rep(0,9))
RMSE_9_res <- c(rep(0,9))
MAE_9 <- c(rep(0,9))
MAE_9_res <- c(rep(0,9))
mean_9_res <- c(rep(0,9))
mean_9_res_FC <- c(rep(0,9))
for (i in 1:9) {
  #RMSE_9[i] <- sqrt( mean( (Data[[i]]$DE - Data[[i]]$DEForecast )^2 ) )
  #RMSE_9_res[i] <- sqrt( mean( ( Residuals_DE[[i]] - Residuals_DE_FC[[i]] )^2 ) )
  
  #MAE_9[i] <- mean( abs( Data[[i]]$DE - Data[[i]]$DEForecast ) ) 
  #MAE_9_res[i] <- mean( abs( Residuals_DE[[i]] - Residuals_DE_FC[[i]] ) ) 
  
  mean_9_res[i] <- mean(Residuals_DE[[i]])
  mean_9_res_FC[i] <- mean(Residuals_DE_FC[[i]])
}

par(mfrow=c(1,2))

plot(RMSE_9,type="l")
lines(RMSE_9_res,col="red")
lines(MAE_9,col="black")
#plot(MAE_9,type="l",ylim=c(0,4))
lines(MAE_9_res,col="red")

plot(mean_9_res,type="l")
lines(mean_9_res_FC,col="red")

plot(mean_9_res-mean_9_res_FC,type="l")


################################################################################
######################## Comparing distributions ###############################
################################################################################

marg_DE_gaus <- MASS::fitdistr(Data_Res$DE_res,densfun = "norma" )
marg_FR_gaus <- MASS::fitdistr(Data_Res$FR_res,densfun = "normal" )
marg_DE_FC_gaus <- MASS::fitdistr(Data_Res$DE_FC_res,densfun = "normal" )
marg_FR_FC_gaus <- MASS::fitdistr(Data_Res$FR_FC_res,densfun = "normal" )
marg_DE_t <- MASS::fitdistr(Data_Res$DE_res,densfun = "t" )
marg_FR_t <- MASS::fitdistr(Data_Res$FR_res,densfun = "t" )
marg_DE_FC_t <- MASS::fitdistr(Data_Res$DE_FC_res,densfun = "t" )
marg_FR_FC_t <- MASS::fitdistr(Data_Res$FR_FC_res,densfun = "t" )
marg_DE_sstd <- fGarch::sstdFit(Data_Res$DE_res)
marg_FR_sstd <- fGarch::sstdFit(Data_Res$FR_res)
marg_DE_FC_sstd <- fGarch::sstdFit(Data_Res$DE_FC_res)
marg_FR_FC_sstd <- fGarch::sstdFit(Data_Res$FR_FC_res)


#AIC
broom::glance(marg_DE_t)
broom::glance(marg_FR_t)
broom::glance(marg_DE_FC_t)
broom::glance(marg_FR_FC_t)
2*marg_DE_sstd$minimum + 2*4
2*marg_FR_sstd$minimum + 2*4
2*marg_DE_FC_sstd$minimum + 2*4
2*marg_FR_FC_sstd$minimum + 2*4


hist(Data_Res$DE_res, pch=20, breaks=120, prob=TRUE, main="",ylim=c(0,0.3),xlim=c(-40,40),xlab="DE_residuals")
curve(dnorm(x, mean = marg_DE_gaus$estimate[1], sd=marg_DE_gaus$estimate[2]),col="blue",lwd=2,add=T)
curve(dt(x, df = marg_DE_t$estimate[3], ncp = marg_DE_t$estimate[1]), col="purple", lwd=2, add=T)
curve(dsstd(x, mean = marg_DE_sstd$estimate[1], sd = marg_DE_sstd$estimate[2], nu =marg_DE_sstd$estimate[3], xi=marg_DE_sstd$estimate[4]), col="red", lwd=2, add=T)
epdfPlot(Data_Res$DE_res,add=T,epdf.lty = 2,epdf.col = "green",epdf.lwd = 2)
legend("topright",legend=c("Empirical","Gaussian","Student's t", "Skewed Student's t"), col=c("green","blue","purple","red"),lty=c(2,1,1,1), lwd=2, )

hist(Data_Res$FR_res, pch=20, breaks=420, prob=TRUE, main="",ylim=c(0,0.3),xlim=c(-40,40),xlab="FR_residuals")
curve(dnorm(x, mean = marg_FR_gaus$estimate[1], sd=marg_FR_gaus$estimate[2]),col="blue",lwd=2,add=T)
curve(dt(x, df = marg_FR_t$estimate[3], ncp = marg_FR_t$estimate[1]), col="purple", lwd=2, add=T)
curve(dsstd(x, mean = marg_FR_sstd$estimate[1], sd = marg_FR_sstd$estimate[2], nu =marg_FR_sstd$estimate[3], xi=marg_FR_sstd$estimate[4]), col="red", lwd=2, add=T)
epdfPlot(Data_Res$FR_res, add=T, epdf.lty = 2, epdf.col = "green", epdf.lwd = 2)
legend("topright",legend=c("Empirical","Gaussian","Student's t", "Skewed Student's t"), col=c("green","blue","purple","red"),lty=c(2,1,1,1), lwd=2, )

hist(Data_Res$DE_FC_res, pch=20, breaks=120, prob=TRUE, main="",ylim=c(0,0.3),xlim=c(-40,40),xlab="DE_FC_residuals")
curve(dnorm(x, mean = marg_DE_FC_gaus$estimate[1], sd=marg_DE_FC_gaus$estimate[2]),col="blue",lwd=2,add=T)
curve(dt(x, df = marg_DE_FC_t$estimate[3], ncp = marg_DE_FC_t$estimate[1]), col="purple", lwd=2, add=T)
curve(dsstd(x, mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4]), col="red", lwd=2, add=T)
epdfPlot(Data_Res$DE_FC_res,add=T,epdf.lty = 2,epdf.col = "green",epdf.lwd = 2)
legend("topright",legend=c("Empirical","Gaussian","Student's t", "Skewed Student's t"), col=c("green","blue","purple","red"),lty=c(2,1,1,1), lwd=2, )

hist(Data_Res$FR_FC_res, pch=20, breaks=620, prob=TRUE, main="",ylim=c(0,0.3),xlim=c(-40,40),xlab="FR_FC_residuals")
curve(dnorm(x, mean = marg_FR_FC_gaus$estimate[1], sd=marg_FR_FC_gaus$estimate[2]),col="blue",lwd=2,add=T)
curve(dt(x, df = marg_FR_FC_t$estimate[3], ncp = marg_FR_FC_t$estimate[1]), col="purple", lwd=2, add=T)
curve(dsstd(x, mean = marg_FR_FC_sstd$estimate[1], sd = marg_FR_FC_sstd$estimate[2], nu =marg_FR_FC_sstd$estimate[3], xi=marg_FR_FC_sstd$estimate[4]), col="red", lwd=2, add=T)
epdfPlot(Data_Res$FR_FC_res,add=T,epdf.lty = 2,epdf.col = "green",epdf.lwd = 2)
legend("topright",legend=c("Empirical","Gaussian","Student's t", "Skewed Student's t"), col=c("green","blue","purple","red"),lty=c(2,1,1,1), lwd=2, )


hist(Data_Res$DE_FC_res,breaks=120,prob=TRUE)
hist(Data_Res$FR_FC_res,breaks=820,prob=TRUE,ylim=c(0,0.3),xlim=c(-40,40))
hist(FR_FC_res,prob=T,breaks=800,ylim=c(0,0.1),xlim=c(-40,40))
lines(density(FR_FC_res))
curve(dsstd(x, mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4]), col="red", lwd=2, add=T)
curve(dsstd(x, mean = marg_FR_sstd$estimate[1], sd = marg_FR_sstd$estimate[2], nu =marg_FR_sstd$estimate[3], xi=marg_FR_sstd$estimate[4]), col="red", lwd=2, add=T)

################################################################################
######################## Fitting copula using sstd #############################
################################################################################


pseudo_DE <- psstd(Data_Res$DE_res, mean = marg_DE_sstd$estimate[1], sd = marg_DE_sstd$estimate[2], nu =marg_DE_sstd$estimate[3], xi=marg_DE_sstd$estimate[4])
pseudo_FR <- psstd(Data_Res$FR_res, mean = marg_FR_sstd$estimate[1], sd = marg_FR_sstd$estimate[2], nu =marg_FR_sstd$estimate[3], xi=marg_FR_sstd$estimate[4])
pseudo_DE_FC <- psstd(Data_Res$DE_FC_res, mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4])
pseudo_FR_FC <- psstd(Data_Res$FR_FC_res, mean = marg_FR_FC_sstd$estimate[1], sd = marg_FR_FC_sstd$estimate[2], nu =marg_FR_FC_sstd$estimate[3], xi=marg_FR_FC_sstd$estimate[4])

fam <- c(0,1,2,3,4,5,6)
DE_sstd_cops <- list()
FR_sstd_cops <- list()
AICs <- c()
for (i in 1:7) {
  #DE_sstd_cops[[i]] <- BiCopSelect(pseudo_DE,pseudo_DE_FC,familyset = fam[i])
  FR_sstd_cops[[i]] <- BiCopSelect(pseudo_FR,pseudo_FR_FC,familyset = fam[i])
  AICs[i] <- FR_sstd_cops[[i]]$AIC
  print(i)
}

DE_sstd_cop <- BiCopSelect(pseudo_DE,pseudo_DE_FC,familyset = NA)
DE_sstd_cop
DE_sstd_cop$taildep
plot(DE_sstd_cop)
plot(DE_Copula_select)

FR_sstd_cop <- BiCopSelect(pseudo_FR,pseudo_FR_FC,familyset = NA)
FR_sstd_cop$AIC
plot(FR_sstd_cop)
FR_sstd_cop$taildep


lambda_DE <- c(lower = fitLambda(cbind(pseudo_DE,pseudo_DE_FC))[2,1],
               upper = fitLambda(cbind(pseudo_DE,pseudo_DE_FC) ,lower.tail = FALSE)[2,1])

lambda_FR <- c(lower = fitLambda(cbind(pseudo_FR,pseudo_FR_FC))[2,1],
               upper = fitLambda(cbind(pseudo_FR,pseudo_FR_FC) ,lower.tail = FALSE)[2,1])





n_simulations <- 100
DE <- list()
CDFs_DE_sstd <- list()
Percentile = c(0.00001,0.0001,0.0005, 0.025, 0.5, 0.975, 0.9995,0.9999,0.99999)
Forecasted_value_DE <- c()
for (j in 1:length(Percentile)) {
  #Forecasted_value_DE[j] <- quantile(spot_data$DEForecast,probs=Percentile[j])
  
  cond_sim_cop_DE <- matrix(nrow=100000,ncol=n_simulations)
  for (i in 1:n_simulations) {
    cond_sim <- BiCopCondSim(100000, cond.val = Percentile[j], cond.var = 2, DE_sstd_cop)
    cond_sim_cop_DE[,i] <- qsstd(cond_sim,mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4])
    #print(i)
  }
  Forecasted_value_DE[j] <- qsstd(Percentile[j], mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4])
  cond_sim_cop <- cbind(cond_sim_cop_DE,Forecasted_value_DE[j])
  ### If we want to reverse back from residuals to prices time series ###
  #cond_sim_cop[,1:n_simulations] <- cond_sim_cop[,1:n_simulations] + fitted_DE
  #cond_sim_cop[,n_simulations+1] <- cond_sim_cop[,n_simulations+1] + fitted_DE_FC
  
  DE[[j]] <- cbind(rowMeans(cond_sim_cop[,1:n_simulations]),cond_sim_cop[,n_simulations+1])
  CDFs_DE_sstd[[j]] <- ecdf(DE[[j]][,1])  
  print(j)
}
names(Forecasted_value_DE) <- names(Forecasted_value)


plot(CDFs_DE_sstd[[1]],xlim=c(-140,170),xlab="x",ylab="P( DE_res < x | DE_FC_res = y )",main="",col=1)
for (j in 2:length(Percentile)) {
  lines(CDFs_DE_sstd[[j]],col=j)
}
spaces <- c("  ","    ","    ","      ","       ","    ","  ","  ","")
temp <- legend("bottomright",legend=c("","","","","","","","",""),lwd=2,col=cols
               ,text.width = strwidth("1,000,000,000,000,000"), xjust = 1, yjust = 1)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c(paste(round(Forecasted_value_DE,digits = 2),paste0(" = Q_",names(Forecasted_value)),spaces)), pos = 2)


L <- c()
U <- c()
means_DE <- c()
for (j in 1:length(Percentile)) {
  sim1 <- BiCopCondSim(1000000, cond.val = Percentile[j], cond.var = 2, DE_Copula_select)
  cond_pdf_DE <- qsstd(sim1, mean = marg_DE_FC_sstd$estimate[1], sd = marg_DE_FC_sstd$estimate[2], nu =marg_DE_FC_sstd$estimate[3], xi=marg_DE_FC_sstd$estimate[4])
  
  L[j] <- quantile(cond_pdf_DE,probs=0.025)
  U[j] <- quantile(cond_pdf_DE,probs=0.975)
  
  means_DE[j] <- mean(cond_pdf_DE)
}

plot(means_DE,type="l",ylim=c(-320,280),axes=FALSE, lwd = 2,
     ylab="E[ DE_res | DE_FC_res ]",xlab="Quantiles for DE_Forecast_residuals")
lines(Forecasted_value_DE,col="purple",lwd=2,lty=2)
lines(L,col="red",lwd=2)
lines(U,col="blue",lwd=2)
axis(2,ylim=c(0,2))
axis(1, at=1:9, labels=names(Forecasted_value))
legend("bottomright",legend=c("E[ DE_res | DE_FC_res ]", "Upper Confidence",
                              "Lower Conficence", "DE_FC_res")
      ,lty=c(1,1,1,2), lwd=2,col=c("black","blue","red","purple"))








n_simulations <- 100
FR <- list()
CDFs_FR_sstd <- list()
Percentile = c(0.00001,0.0001,0.0005, 0.025, 0.5, 0.975, 0.9995,0.9999,0.99999)
Forecasted_value_FR <- c()
for (j in 1:length(Percentile)) {
  #Forecasted_value_FR[j] <- quantile(spot_data$FRForecast,probs=Percentile[j])
  
  cond_sim_cop_FR <- matrix(nrow=100000,ncol=n_simulations)
  for (i in 1:n_simulations) {
    cond_sim <- BiCopCondSim(100000, cond.val = Percentile[j], cond.var = 2, FR_sstd_cop)
    cond_sim_cop_FR[,i] <- qsstd(cond_sim,mean = marg_FR_FC_sstd$estimate[1], sd = marg_FR_FC_sstd$estimate[2], nu =marg_FR_FC_sstd$estimate[3], xi=marg_FR_FC_sstd$estimate[4])
    #print(i)
  }
  Forecasted_value_FR[j] <- qsstd(Percentile[j], mean = marg_FR_FC_sstd$estimate[1], sd = marg_FR_FC_sstd$estimate[2], nu =marg_FR_FC_sstd$estimate[3], xi=marg_FR_FC_sstd$estimate[4])
  cond_sim_cop <- cbind(cond_sim_cop_FR,Forecasted_value_FR[j])
  ### If we want to reverse back from residuals to prices time series ###
  #cond_sim_cop[,1:n_simulations] <- cond_sim_cop[,1:n_simulations] + fitted_DE
  #cond_sim_cop[,n_simulations+1] <- cond_sim_cop[,n_simulations+1] + fitted_DE_FC
  
  FR[[j]] <- cbind(rowMeans(cond_sim_cop[,1:n_simulations]),cond_sim_cop[,n_simulations+1])
  CDFs_FR_sstd[[j]] <- ecdf(FR[[j]][,1])  
  print(j)
}
names(Forecasted_value_FR) <- names(Forecasted_value)


plot(CDFs_FR_sstd[[1]],xlim=c(-100,200),xlab="x",ylab="P( FR_res < x | FR_FC_res = y )",main="",col=1)
for (j in 2:length(Percentile)) {
  lines(CDFs_FR_sstd[[j]],col=j)
}
spaces <- c("  ","    ","    ","     ","       ","    ","  ","  ","")
temp <- legend("bottomright",legend=c("","","","","","","","",""),lwd=2,col=cols
               ,text.width = strwidth("1,000,000,000,000,000"), xjust = 1, yjust = 1)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c(paste(round(Forecasted_value_FR,digits = 2),paste0(" = Q_",names(Forecasted_value)),spaces)), pos = 2)



L <- c()
U <- c()
means_FR <- c()
for (j in 1:length(Percentile)) {
  sim1 <- BiCopCondSim(1000000, cond.val = Percentile[j], cond.var = 2, FR_Copula_select)
  cond_pdf_FR <- qsstd(sim1, mean = marg_FR_FC_sstd$estimate[1], sd = marg_FR_FC_sstd$estimate[2], nu = marg_FR_FC_sstd$estimate[3], xi = marg_FR_FC_sstd$estimate[4])
  
  L[j] <- quantile(cond_pdf_FR,probs=0.025)
  U[j] <- quantile(cond_pdf_FR,probs=0.975)
  
  means_FR[j] <- mean(cond_pdf_FR)
}

plot(means_FR,type="l",ylim=c(-320,280),axes=FALSE, lwd = 2,
     ylab="E[ FR_res | FR_FC_res ]",xlab="Quantiles for FR_Forecast_residuals")
lines(Forecasted_value_FR,col="purple",lwd=2,lty=2)
lines(L,col="red",lwd=2)
lines(U,col="blue",lwd=2)
axis(2,ylim=c(0,2))
axis(1, at=1:9, labels=c("Q_0.001%"," ","Q_0.05%"," ","Q_50%"," ","Q_99.95.%"," ","99.999%"))
legend("bottomright",legend=c("E[ FR_res | FR_FC_res ]", "Upper Confidence",
                              "Lower Conficence", "FR_FC_res")
       ,lty=c(1,1,1,2), lwd=2,col=c("black","blue","red","purple"))




################################################################################
###### Computing Conditional Distributions using numerical differentiation #####
################################################################################

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

Y = list(c(0,10,-10,20,-20,30,-30,40,-40),c(0,10,-10,25,-25,50,-50,75,-75))
Legend = list(c("FC = -40","FC = -30","FC = -20","FC = -10","FC = 0","FC = 10",
                "FC = 20","FC = 30","FC = 40")
              ,c("FC = -75","FC = -50","FC = -25","FC = -10","FC = 0","FC = 10",
                 "FC = 25","FC = 50","FC = 75"))
X_lim = list(c(-60,60),c(-100,100))
CCC = list()
navn <- c("DE","FR")
for (i in 1:2) {
  
  Data = tibble(X = Data_Res[,i+1], Y = Data_Res[,i+3])
  
  F_X = ecdf(Data$X)
  F_Y = ecdf(Data$Y)
  
  
  U = Data %>% mutate(U = F_X(X), V = F_Y(Y)) %>% select(U,V)
  fitted = fitCopula(tCopula(), U, method = "itau")
  CCC[[i]] = fitted
  
  x_seq = seq(from = X_lim[[i]][1], to = X_lim[[i]][2], length.out = 1000)
  
  y = Y[[i]][1]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  plot(x_seq, `P(X<=x|Y=y)`, type = "l", col = "yellow",xlab = "x", main=navn[i])

  y = Y[[i]][2]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "green")
  
  y = Y[[i]][3]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "orange")
  
  y = Y[[i]][4]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "lightblue")
  
  y = Y[[i]][5]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "red")
  
  y = Y[[i]][6]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "blue")
  
  y = Y[[i]][7]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "magenta")

  y = Y[[i]][8]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "darkblue")
  
  y = Y[[i]][9]
  `P(X<=x|Y=y)` = `F_X|Y`(x_seq,y,F_X,F_Y,C_hat)
  lines(x_seq, `P(X<=x|Y=y)`, col = "purple")
  
  
  legend("bottomright", legend=Legend[[i]],
         col=c("purple","magenta","red", "orange","yellow","green","lightblue","blue","darkblue"), lty=1, cex=0.8)
  
}




################################################################################
############################# Spread Analysis ##################################
################################################################################

spread <- spot_data$DE - spot_data$FR
spread_FC <- spot_data$DEForecast - spot_data$FRForecast
plot(spot_data$StartUTC,spread,type="l",xlab="Time",main="Spot Spread",ylab="Price")
lines(spot_data$StartUTC, spread_FC,col="red")
#plot(spot_data$StartUTC,spread_FC,type="l",xlab="Time",main="Forecasted Spot Spread",ylab="Price")

hist(spread,breaks=80,main="Spot Spread",xlab="Price",xlim=c(-80,80))
hist(spread_FC,breaks=80,main="Forecasted Spot Spread",xlab="Price",xlim=c(-80,80))

plot(spot_data$StartUTC,spot_data$Spread-spot_data$Spread_FC,type="l",xlab="Time",ylab="Euros")
plot(spot_data$StartUTC,spot_data$FR-spot_data$FRForecast,type="l")

hist(spot_data$Spread-spot_data$Spread_FC,breaks=138,xlim=c(-50,50),xlab="Price",main="")

acf(spread,lag.max=300)
acf(spread_FC,lag.max=300)


spot_data <- spot_data %>% mutate(Spread=spread, Spread_FC = spread_FC)

Spread_model <- lm(spot_data$Spread~
                      time(spot_data[,1])+
                      # I(time(Data[[i]][,1])^2)+
                      #I(time(Data[[i]][,1])^3)+
                      cos((2*pi/length(spot_data[,1])) * I(time(spot_data[,1])))+
                      sin((2*pi/length(spot_data[,1])) * I(time(spot_data[,1])))+
                      #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                      #cos(((2*365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                      #sin(((2*365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                      #cos(((365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                      #sin(((365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                      #factor(Data[[i]]$Yd)+
                      #factor(Data[[i]]$yd)+
                      factor(spot_data$wd)+
                      factor(spot_data$dd)
)

Spread_FC_model <- lm(spot_data$Spread_FC~
                     time(spot_data[,1])+
                     # I(time(Data[[i]][,1])^2)+
                     #I(time(Data[[i]][,1])^3)+
                     cos((2*pi/length(spot_data[,1])) * I(time(spot_data[,1])))+
                     sin((2*pi/length(spot_data[,1])) * I(time(spot_data[,1])))+
                     #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                     cos(((2*365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                     sin(((2*365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                     cos(((365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                     sin(((365*2)*pi/length(spot_data[,1]))*I(time(spot_data[,1])))+
                     #factor(Data[[i]]$Yd)+
                     #factor(Data[[i]]$yd)+
                     factor(spot_data$wd)+
                     factor(spot_data$dd)
)
summary(Spread_model)
summary(Spread_FC_model)

acf(Spread_model$residuals,lag.max = 300)
acf(Spread_FC_model$residuals,lag.max = 300)

Spread_sarma_1 <- arima(Spread_model$residuals, order = c(0,0,0), seasonal = list(order=c(1,0,0),period=24))
Spread_FC_sarma_1 <- arima(Spread_FC_model$residuals, order = c(0,0,0), seasonal = list(order=c(1,0,0),period=24)) 

acf(Spread_sarma_1$residuals,lag.max=30)
acf(Spread_FC_sarma_1$residuals,lag.max=30)

Spread_arima <- auto.arima(Spread_sarma_1$residuals)
Spread_FC_arima <- auto.arima(Spread_FC_sarma_1$residuals)

summary(Spread_arima)
summary(Spread_FC_arima)

acf(Spread_arima$residuals,lag.max = 300)
acf(Spread_FC_arima$residuals,lag.max = 300)

spread_res <- Spread_arima$residuals
spread_FC_res <- Spread_FC_arima$residuals

hist(spread_res,breaks=30)
hist(spread_FC_res,breaks=30)

plot(spread_FC_res,type="l")

res_spread <- as.data.frame(cbind(spread_res,spread_FC_res))

spread_no_zero <- res_spread[which(spot_data$Spread_FC == 0),]

hist(spread_no_zero$spread_res,breaks=30)
plot(spread_no_zero$spread_res)

spread_cop_select <- BiCopSelect(pobs(res_spread$spread_res),pobs(res_spread$spread_FC_res),familyset = NA)
spread_cop_select
plot(spread_cop_select)
spread_cop_select$taildep


### DE prediction analysis when spread = 0 ###
index_spread_zero <- which(spot_data$Spread == 0)
Data_res_no_spread <- Data_Res[index_spread_zero,]
View(Data_res_no_spread)
DE_cop_no_spread <- BiCopSelect(pobs(Data_res_no_spread$DE_res),pobs(Data_res_no_spread$DE_FC_res),familyset = NA)
DE_cop_no_spread
plot(DE_cop_no_spread)
DE_cop_no_spread$taildep
sqrt(mean((Data_res_no_spread$DE_res - Data_res_no_spread$DE_FC_res)^2))
sqrt(mean((Data_Res$DE_res - Data_Res$DE_FC_res)^2))

sqrt(mean((spot_data[index_spread_zero,2]-spot_data[index_spread_zero,4])^2))
sqrt(mean((spot_data[,2]-spot_data[,4])^2))



Data_Spread <- list()
Data_Spread_FC <- list()
for (i in 1:9) {
  Data_Spread[[i]] <- Data[[i]]$DE-Data[[i]]$FR
  Data_Spread_FC[[i]] <- Data[[i]]$DEForecast-Data[[i]]$FRForecast
}

Y_length = c(length(Y16[,1]),length(Y16[,1]), length(Y17[,1]),length(Y17[,1]), length(Y18[,1]),length(Y18[,1]), 
             length(Y19[,1]), length(Y19[,1]),length(Y20[,1]),length(Y20[,1]))

models_Spread = list()
for (i in 1:9) {
  models_Spread[[i]] <- lm(Data_Spread[[i]] ~
                            time(Data[[i]][,1])+
                            # I(time(Data[[i]][,1])^2)+
                            #I(time(Data[[i]][,1])^3)+
                            cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
                            sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                            cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                            #factor(Data[[i]]$Yd)+
                            #factor(Data[[i]]$yd)+
                            factor(Data[[i]]$wd)+
                            factor(Data[[i]]$dd)
  )
}


models_Spread_FC = list()
for (i in 1:9) {
  models_Spread_FC[[i]] <- lm(Data_Spread_FC[[i]] ~
                             time(Data[[i]][,1])+
                             # I(time(Data[[i]][,1])^2)+
                             #I(time(Data[[i]][,1])^3)+
                             cos( ( 2*pi/Y_length[i] ) * I(time(Data[[i]][,1])) )+
                             sin((2*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                             #cos(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #sin(((2*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #cos(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #sin(((4*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #cos(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #sin(((12*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #cos(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             #sin(((52*2)*pi/Y_length[i])*I(time(spot_data[,1])))+
                             cos(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                             sin(((2*365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                             cos(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                             sin(((365*2)*pi/Y_length[i])*I(time(Data[[i]][,1])))+
                             #factor(Data[[i]]$Yd)+
                             #factor(Data[[i]]$yd)+
                             factor(Data[[i]]$wd)+
                             factor(Data[[i]]$dd)
  )
}

head(models_Spread[[1]]$residuals)
head(models_Spread_FC[[1]]$residuals)

acf(models_Spread[[1]]$residuals)

par(mfrow=c(3,3))
sarma24_spread = list()
sarma24_spread_FC = list()
for (i in 1:9) {
  #sarma24_spread[[i]] <- arima(models_Spread[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  sarma24_spread_FC[[i]] <- arima(models_Spread_FC[[i]]$residuals, order = od[[i]], seasonal = list(order=c(1,0,0),period=24))
  Acf(sarma24_spread_FC[[i]]$residuals,lag.max=400)
}

head(sarma24_spread[[1]]$residuals)
head(sarma24_spread_FC[[1]]$residuals)

sarma168_Spread = list() 
sarma168_Spread_FC = list() 
for (i in 1:9) {
  #sarma168_Spread[[i]] <- arima(sarma24_spread[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
  sarma168_Spread_FC[[i]] <- arima(sarma24_spread_FC[[i]]$residuals, seasonal = list(order = c(1,0,0), period=168))
  acf(sarma168_Spread_FC[[i]]$residuals,lag.max=400)
}

head(sarma168_Spread[[1]]$residuals)
head(sarma168_Spread_FC[[1]]$residuals)

ARMA_Spread = list()
ARMA_Spread_FC = list()
for (i in 1:9) {
  #ARMA_Spread[[i]] <- auto.arima(sarma168_Spread[[i]]$residuals)
  ARMA_Spread_FC[[i]] <- auto.arima(sarma168_Spread_FC[[i]]$residuals)
  Acf(ARMA_Spread_FC[[i]]$residuals)

}

head(ARMA_Spread[[1]]$residuals)
head(ARMA_Spread_FC[[1]]$residuals)

Spread_arma_residuals = list()
Spread_FC_arma_residuals = list()
for (i in 1:9) {
  #Spread_arma_residuals[[i]] <- ARMA_Spread[[i]]$residuals
  Spread_FC_arma_residuals[[i]] <- ARMA_Spread_FC[[i]]$residuals
}

Spread_residuals <- unlist(Spread_arma_residuals)
Spread_FC_residuals <- unlist(Spread_FC_arma_residuals)

Data_Spread_Res <- data.frame(StartUTC=spot_data$StartUTC,Spread_residuals,Spread_FC_residuals)
#View(Data_Spread_Res)



spread_9_cop_select <- BiCopSelect(pobs(Data_Spread_Res$Spread_residuals),pobs(Data_Spread_Res$Spread_FC_residuals),familyset = NA)
spread_9_cop_select
plot(spread_9_cop_select,xlab="Spread", ylab="Spread FC")
spread_9_cop_select$taildep


## Comparison for spread prediction error for whole period ## 
sqrt(mean((spread_res-spread_FC_res)^2))
sqrt(mean((Data_Res$DE_res - Data_Res$DE_FC_res)^2))


plot(spot_data$StartUTC ,spot_data$FR-spot_data$FRForecast,type="l")

hist(Data_Res$DE_res-Data_Res$FR_res,breaks=3000,xlim=c(-30,30))
hist(Data_Res$DE_FC_res-Data_Res$FR_FC_res,breaks=3000,xlim=c(-30,30))
hist(Data_Res$DE_res-Data_Res$DE_FC_res,breaks=100,xlim=c(-30,30))
hist(Data_Res$DE_FC_res-Data_Res$FR_FC_res,breaks=300,xlim=c(-30,30))



ggseasonplot(ts(spot_data$DE[(168*30):(168*60)],frequency = 168))
