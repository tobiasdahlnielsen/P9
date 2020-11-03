## Load packages
source("Lib.R")

## Load data
spot_data <- as.data.frame(read_csv("spot_data.csv"))
spot_data <- spot_data %>% mutate(StartUTC = as.POSIXct(StartUTC, tz = "UTC", format = "%Y-%m-%d %H:%M"))

## remove seasonality
data <- spot_data[,1]

periode <- 365*24
for (i in 1:4) {
  model <- glm(spot_data[,i+1]~
                 time(spot_data[,1])+
                  I(time(spot_data[,1])^2)+
                 #I(time(spot_data[,1])^3)+
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
# print( summary(model))
data <- cbind(data,model$residuals) 

}
data <- as.data.frame(data);names(data) <- c(names(spot_data))

pre <- auto.arima(data$DE,d=0)

sarima(data$DE,p=3,d=0,q=2,P=1,S=24)

# par(mfrow=c(2,1))
# plot(spot_data[,3],type = "l")
# plot(model$residuals,type="l")
# par(mfrow=c(1,1))
# plot(spot_data$StartUTC,model$fitted.values,type = "l")
