source("lib.R")
library(readr)
spot_data <- read_csv("spot_data.csv")


periode <- 365*25
for (i in 1:4) {
  model <- lm(spot_data[,i+1]~
                time(spot_data[,1])+
                I(time(spot_data[,1])^2)+
                I(time(spot_data[,1])^3)+
                I(time(spot_data[,1])^4)+
                cos((2*pi/periode)*I(time(spot_data[,1])))+
                sin((2*pi/periode)*I(time(spot_data[,1])))+
                cos((4*pi/periode)*I(time(spot_data[,1])))+
                sin((4*pi/periode)*I(time(spot_data[,1])))+
                cos((8*pi/periode)*I(time(spot_data[,1])))+
                sin((8*pi/periode)*I(time(spot_data[,1]))))
  acf(model$residuals)
  arma <- auto.arima(model$residuals,seasonal = TRUE)
  acf(arma$residuals)
}



model <- lm(spot_data[,2]~
              time(spot_data[,1])+
              I(time(spot_data[,1])^2)+
              I(time(spot_data[,1])^3)+
              I(time(spot_data[,1])^4)+
              cos((2*pi/periode)*I(time(spot_data[,1])))+
              sin((2*pi/periode)*I(time(spot_data[,1])))+
              cos((4*pi/periode)*I(time(spot_data[,1])))+
              sin((4*pi/periode)*I(time(spot_data[,1])))+
              cos((8*pi/periode)*I(time(spot_data[,1])))+
              sin((8*pi/periode)*I(time(spot_data[,1])))+
              cos((16*pi/periode)*I(time(spot_data[,1])))+
              sin((16*pi/periode)*I(time(spot_data[,1])))+
              cos((32*pi/periode)*I(time(spot_data[,1])))+
              sin((32*pi/periode)*I(time(spot_data[,1]))))

par(mfrow=c(1,1))
plot(spot_data$StartUTC[tid],spot_data$DE[tid],type="l")
plot(spot_data$StartUTC[tid],model$residuals[tid],type="l")

plot(spot_data$StartUTC,model$fitted.values,type="l")


arma <- arima(model1$residuals,order=c(3,0,1),seasonal=c(24,0,0))
