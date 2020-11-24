## Load packages
source("Lib.R")
load("~/P9/armagarchparm.RData")
#load("~/P9/5arga.RData")
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
#ARMAtest <- auto.arima(model$residuals)
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = bp[[i]][3:4],
                                         submodel = NULL,
                                         external.regressors = NULL,
                                         variance.targeting = FALSE),

                   mean.model     = list(armaOrder =  c(bp[[i]][1:2]),
                                         external.regressors = NULL,
                                         distribution.model = "norm",
                                         start.pars = list(),
                                         fixed.pars = list()))

garch <- ugarchfit(spec = spec, data = model$residuals,solver = "hybrid", solver.control = list(trace=0))
#sarma <- arima(ARMAtest$residuals,seasonal = list(order=c(1,0,0),period=24))
#print(summary(ARMAtest))
#acf(sarma$residuals)
#Acf(ARMAtest$residuals)
data <- cbind(data,garch@fit$residuals)
datafit <- cbind(datafit,garch@fit$fitted.values)
}
data <- as.data.frame(data);names(data) <- c(names(spot_data))[1:(i+1)]
datafit <- as.data.frame(datafit);names(datafit) <- c(names(spot_data))[1:(i+1)]

#auto.arima(model$residuals)
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