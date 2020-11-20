## Load packages
source("Lib.R")

## Load data
spot_data <- as.data.frame(read_csv("spot_data.csv"))
spot_data <- spot_data %>% mutate(StartUTC = as.POSIXct(StartUTC, tz = "UTC", format = "%Y-%m-%d %H:%M"))

## remove seasonality
{data <- spot_data[,1]
datafit <- spot_data[,1]
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
 
#plot(model$residuals,type="l")
ARMAtest <- auto.arima(model$residuals)
spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   
                   mean.model     = list(armaOrder = c(ARMAtest$arma[1], ARMAtest$arma[2]), 
                                         external.regressors = NULL, 
                                         distribution.model = "norm", 
                                         start.pars = list(), 
                                         fixed.pars = list()))

garch <- ugarchfit(spec = spec, data = spot_data[,i+1], solver.control = list(trace=0))
#sarma <- arima(ARMAtest$residuals,seasonal = list(order=c(1,0,0),period=24))
#print(ARMAtest)
#acf(sarma$residuals)
#Acf(ARMAtest$residuals)
data <- cbind(data,garch@fit$residuals)
datafit <- cbind(datafit,garch@fit$fitted.values)
}
data <- as.data.frame(data);names(data) <- c(names(spot_data))[1:(i+1)]
datafit <- as.data.frame(datafit);names(datafit) <- c(names(spot_data))[1:(i+1)]

#auto.arima(model$residuals)
}


# periode <- 365*24
# for (i in 1:4) {
#   model <- glm(spot_data[,i+1]~
#                  time(spot_data[,1])+
#                  I(time(spot_data[,1])^2)+
#                  I(time(spot_data[,1])^3)+
#                  I(time(spot_data[,1])^4)+
#                  I(time(spot_data[,1])^5)+
#                  #I(time(spot_data[,1])^6)+
#                  #I(time(spot_data[,1])^7)+
#                  #I(time(spot_data[,1])^8)+
#                  cos((2*pi/periode)*I(time(spot_data[,1])))+
#                  sin((2*pi/periode)*I(time(spot_data[,1])))+
#                  cos(((2*2)*pi/periode)*I(time(spot_data[,1])))+
#                  sin(((2*2)*pi/periode)*I(time(spot_data[,1])))+
#                  cos(((4*2)*pi/periode)*I(time(spot_data[,1])))+
#                  sin(((4*2)*pi/periode)*I(time(spot_data[,1])))+
#                  cos(((12*2)*pi/periode)*I(time(spot_data[,1])))+
#                  sin(((12*2)*pi/periode)*I(time(spot_data[,1])))+
#                  cos(((52*2)*pi/periode)*I(time(spot_data[,1])))+
#                  sin(((52*2)*pi/periode)*I(time(spot_data[,1])))+
#                  cos(((365*2)*pi/periode)*I(time(spot_data[,1])))+
#                  sin(((365*2)*pi/periode)*I(time(spot_data[,1])))
#   )
# plot(spot_data[10000:10500,i+1],type="l");lines(model$fitted.values[10000:12000],col="red")
#   
# }