## Load packages
source("Lib.R")
load("~/Rstudio Git/P9/armagarchparm.RData")
load("~/Rstudio Git/P9/5arga.RData")
## Load data
spot_data <- as.data.frame(read_csv("spot_data.csv"))
spot_data <- spot_data %>% mutate(StartUTC = as.POSIXct(StartUTC, tz = "UTC", format = "%Y-%m-%d %H:%M"))

## remove seasonality



{
dd <- seasonaldummy(ts(spot_data$DE,frequency = 24))
  wh = rep(2,length(spot_data$wd))
  wh[which(spot_data$wd < 6)] = 1
  wh
spot_data <- spot_data %>% mutate(hd = hour(StartUTC)+1, wd = wday(StartUTC), md = day(StartUTC), yd = month(StartUTC))



spot_data$wd
data <- spot_data[,1]
datafit <- spot_data[,1]
periode <- 365*24


spot_data <- spot_data[17543:26278,]
for (i in 1:4) {
  model <- glm(spot_data[,i+1]~
                 time(spot_data[,1])+
                 I(time(spot_data[,1])^2)+
                 I(time(spot_data[,1])^3)+
                 I(time(spot_data[,1])^4)+
                 I(time(spot_data[,1])^5)+
                 I(time(spot_data[,1])^6)+
                 #I(time(spot_data[,1])^7)+
                 #I(time(spot_data[,1])^8)+
                 # cos((2*pi/periode)*I(time(spot_data[,1])))+
                 # sin((2*pi/periode)*I(time(spot_data[,1])))+
                 # cos(((2*2)*pi/periode)*I(time(spot_data[,1])))+
                 # sin(((2*2)*pi/periode)*I(time(spot_data[,1])))+
                 # cos(((4*2)*pi/periode)*I(time(spot_data[,1])))+
                 # sin(((4*2)*pi/periode)*I(time(spot_data[,1])))+
                 # cos(((12*2)*pi/periode)*I(time(spot_data[,1])))+
                 # sin(((12*2)*pi/periode)*I(time(spot_data[,1])))+
                 # cos(((52*2)*pi/periode)*I(time(spot_data[,1])))+
                 # sin(((52*2)*pi/periode)*I(time(spot_data[,1])))+
                 #  cos(((365*2)*pi/periode)*I(time(spot_data[,1])))+
                 #  sin(((365*2)*pi/periode)*I(time(spot_data[,1])))+
                 # cos(((2*365*2)*pi/periode)*I(time(spot_data[,1])))+
                 # sin(((2*365*2)*pi/periode)*I(time(spot_data[,1])))+
                 factor(spot_data$hd)+
                 factor(spot_data$wd)+
                 factor(spot_data$md)+
                 factor(spot_data$yd)
  )
summary(model)
  # plot(spot_data[3600:4400,2],type="l")
  # lines(model$fitted.values[3600:4400],col="red")
  # plot(model$residual,type="l")
acf(model$residuals,lag.max = 100)

model2 <- auto.arima(model$residuals)

plot(model2$residuals[1:1000],type="l")
lines(model$residuals[1:1000],col="red")
acf(model2$residuals,lag.max = 300)

sarma2 <- arima(model2$residuals,seasonal = list(order=c(3,0,0),period=24))
summary(sarma2)
acf(sarma2$residuals,lag.max = 3000)
Box.test(sarma2$residuals,type = "Ljung-Box", lag=4)
plot(sarma2$residuals[1:1000],type="l")
hist(sarma2$residuals[1:1000],breaks = 80)
  
ARMAtest <- auto.arima(ts(model$residuals,frequency = 24))
summary(ARMAtest)
acf(ARMAtest$residuals,lag.max = 800)
plot(ARMAtest$residuals,type="l")


sarma <- arima(model$residuals,seasonal = list(order=c(3,0,0),period=24))
acf(sarma$residuals,lag.max=400)

model3 <- auto.arima(sarma$residuals)
summary(model3)
acf(model3$residuals,lag.max=300)

arma <- permutations(6,2,v=0:5,repeats.allowed = TRUE)

for (i in 1:dim(arma)[1]) {
  vari <- arima(sarma$residuals,order = c(arma[i,1],0,arma[i,2]))$residuals
  Acf(vari,main=arma[i,],lag.max = 300)
  pacf(vari,main=arma[i,],lag.max = 300)
}


Box.test(model3$residuals,type="Ljung-Box", lag=48)


summary(sarma)
acf(sarma$residuals,lag.max = 1000)

spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(0,1),
                                         submodel = NULL,
                                         external.regressors = NULL,
                                         variance.targeting = FALSE),

                   
                   mean.model     = list(armaOrder =  c(5,1),
                                         external.regressors = NULL,
                                         distribution.model = "norm",
                                         start.pars = list(),
                                         fixed.pars = list()))


garch <- ugarchfit(spec = spec, data = sarma$residuals,solver = "hybrid", solver.control = list(trace=0))
#sarma <- arima(garch@fit$residuals,seasonal = list(order=c(5,0,0),period=24))
acf(garch@fit$residuals,lag.max = 300)


data <- cbind(data,garch@fit$residuals)
#datafit <- cbind(datafit,garch@fit$fitted.values)
}
data <- as.data.frame(data);names(data) <- c(names(spot_data))[1:(i+1)]
datafit <- as.data.frame(datafit);names(datafit) <- c(names(spot_data))[1:(i+1)]


}

# parm <- permutations(5,4,repeats.allowed = T)
# 
# ekstra <-unique(c(which(parm[,3]==c(4)),which(parm[,3]==c(5)),which(parm[,4]==c(4)),which(parm[,4]==c(5))))
# parm <- parm[-ekstra,]
# bp <- list()
# allic <- list()
# tictoc::tic()
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
#     ic <- c()
#       for (j in 1:dim(parm)[1]) {
#       spec <- ugarchspec(variance.model = list(model = "sGARCH",
#                                          garchOrder = parm[j,3:4],
#                                          submodel = NULL,
#                                          external.regressors = NULL,
#                                          variance.targeting = FALSE),
# 
#                    mean.model     = list(armaOrder = parm[j,1:2],
#                                          external.regressors = NULL,
#                                          distribution.model = "norm",
#                                          start.pars = list(),
#                                          fixed.pars = list()))
# 
#       garch <- ugarchfit(spec = spec, data = model$residuals,solver = "hybrid", solver.control = list(tol = 1e-12,trace=0))
#         if (garch@fit$convergence==0) {
#           ic <- c(ic,infocriteria(garch)[1])
#         }
#       else{
#         ic <- c(ic,NA)
#       }
#     print(paste(i,j,garch@fit$convergence))
#       }
#   allic[[i]] <- ic
#   bp[[i]] <- parm[which(ic==min(ic,na.rm = T)),]
# }
# tictoc::toc()


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