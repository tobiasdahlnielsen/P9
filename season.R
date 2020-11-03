## Load packages
source("Lib.R")

## Load data
spot_data <- as.data.frame(read_csv("spot_data.csv"))
spot_data <- spot_data %>% mutate(StartUTC = as.POSIXct(StartUTC, tz = "UTC", format = "%Y-%m-%d %H:%M"))

## remove seasonality
{data <- spot_data[,1]

periode <- 365*24
for (i in 1:4) {
  model <- glm(spot_data[,i+1]~
                 time(spot_data[,1])+
                 I(time(spot_data[,1])^2)+
                 I(time(spot_data[,1])^3)+
                 I(time(spot_data[,1])^4)+
                 I(time(spot_data[,1])^5)+
                 #I(time(spot_data[,1])^6)+
                 #I(time(spot_data[,1])^7)+
                 #I(time(spot_data[,1])^8)+
                 cos((2*pi/periode)*I(time(spot_data[,1])))+
                 sin((2*pi/periode)*I(time(spot_data[,1])))+
                 cos(((2*2)*pi/periode)*I(time(spot_data[,1])))+
                 sin(((2*2)*pi/periode)*I(time(spot_data[,1])))+
                 cos(((4*2)*pi/periode)*I(time(spot_data[,1])))+
                 sin(((4*2)*pi/periode)*I(time(spot_data[,1])))+
                 cos(((12*2)*pi/periode)*I(time(spot_data[,1])))+
                 sin(((12*2)*pi/periode)*I(time(spot_data[,1])))+
                 cos(((52*2)*pi/periode)*I(time(spot_data[,1])))+
                 sin(((52*2)*pi/periode)*I(time(spot_data[,1])))+
                 cos(((365*2)*pi/periode)*I(time(spot_data[,1])))+
                 sin(((365*2)*pi/periode)*I(time(spot_data[,1])))
  )
 #print( summary(model))
data <- cbind(data,model$residuals) 
#plot(model$residuals,type="l")
ARMAtest <- auto.arima(model$residuals)
seaarma <- arima(ARMAtest$residuals,seasonal = list(order=c(1,0,0),period=24))
#print(ARMAtest)
acf(seaarma$residuals)
#Acf(ARMAtest$residuals)
}
data <- as.data.frame(data);names(data) <- c(names(spot_data))

#auto.arima(model$residuals)
}
## Remove seasonality diffed data

datadif <- as.data.frame(cbind(diff(data$DE),diff(data$FR),diff(data$DEForecast),diff(data$FRForecast)))
names(datadif) <- c(names(spot_data)[2:5])

periode <- 365*24
for (i in 1:4) {
  model <- glm(datadif[,i]~-1+
                 time(datadif[,1])+
                 I(time(datadif[,1])^2)+
                 I(time(datadif[,1])^3)+
                 I(time(datadif[,1])^4)+
                 I(time(datadif[,1])^5)+
                 I(time(datadif[,1])^6)+
                 I(time(datadif[,1])^7)+
                 I(time(datadif[,1])^8)+
                 cos((2*pi/periode)*I(time(datadif[,1])))+
                 sin((2*pi/periode)*I(time(datadif[,1])))+
                 cos(((2*2)*pi/periode)*I(time(datadif[,1])))+
                 sin(((2*2)*pi/periode)*I(time(datadif[,1])))+
                 cos(((4*2)*pi/periode)*I(time(datadif[,1])))+
                 sin(((4*2)*pi/periode)*I(time(datadif[,1])))+
                 cos(((12*2)*pi/periode)*I(time(datadif[,1])))+
                 sin(((12*2)*pi/periode)*I(time(datadif[,1])))+
                 cos(((52*2)*pi/periode)*I(time(datadif[,1])))+
                 sin(((52*2)*pi/periode)*I(time(datadif[,1])))+
                 cos(((365*2)*pi/periode)*I(time(datadif[,1])))+
                 sin(((365*2)*pi/periode)*I(time(datadif[,1])))
  )
  print( summary(model))
  data <- cbind(data,model$residuals) 
  plot(model$residuals,type="l")
}








test <- auto.arima(diff(spot_data$DE))
Acf(test$residuals)

# par(mfrow=c(2,1))
# plot(spot_data[,3],type = "l")
# plot(model$residuals,type="l")
# par(mfrow=c(1,1))
# plot(spot_data$StartUTC,model$fitted.values,type = "l")
