library(tidyverse)
library(forecast)
library(seasonal)
library(fpp2)
library(urca)
library(gridExtra)
library(lubridate)
library(astsa)
library(rugarch)
library(tsdl)

#==================
# loading JJ data

autoplot(jj) + autolayer(ma(jj,order = 4)) + 
  theme(legend.title = element_blank(), legend.position  = "bottom")

autoplot(decompose(jj, type = "multiplicative"))

#===================
#  visualizing the flu data

autoplot(flu)

train<- window()

#==============
# visualizing globtemp
autoplot(globtemp)
autoplot(globtempl)

#=======================
autoplot(star)

star %>%  ggtsdisplay()

model<-auto.arima(star)


#====================================

#stanioraity
# we do not want any systematic change in the variation and the mean
# no trend no variation, and no periodic flucutation, sesonality

#=====================================
#Autocovariance  function 
#random variable is a function taht goes from sample space to real numbers
# sample sace are all possible outcomes of the experiment 
#once we do the experiment, x become 20 randomness is gone and what we have 
# is the realization of the random variable.

# Covariance 
# if we have two random variables x and we can use covariance 
# to measure the linear dipendence between two random variables 

# Stoachastic processes 
# collection of random variables, and give a sequence, we call it a stochastic process
#in deterministic process, you know where you are at every step.
# in stocahastic at every step we dont know where we are, there is some randomness 

#Time series is a realization of a stochastic process
#realization of x1 is first data point, x2 realization second data point and so on

#Autocovariane function 
# autcovariane is the covaraiane of two random variables at difference time
# this works because we assume we are working with stationary time series.
# so the properties of a part of time series is the same as another.
# therefore the covariance of two points in time is the same as another two points in time
# from same time series.

# Autocovariance coefficients.
#autocovariane coefficients at different lags.

ts<- ts(rnorm(1000))
ggAcf(ts)

 #autocorrelation = covariance at time k/covariance at time 0
acf(ts)

#random walk

# xt = xt-1 + zt(white noise)
# therfore xt = sum(zi) we jut accumulate noises until time t.
#expectation of xt = mean of sum(zi), so it changes base on the location
# variation of xt = var of sum(zi), assuming that zi are independent
#therefore a random walk is not stationary, since mean and variane change in time.


x = 0
zi =rnorm(1000)
y = matrix(0,1000,2)
y[1,1] = x
for (i in seq(2,1000) ){

y[i,1] = x + zi[i] 
x = x + zi[i]
  
}

autoplot(ts(y))

model<- auto.arima(ts(y))

ts(y) %>% ggtsdisplay()
model %>% checkresiduals()
forecast(model,100) %>% autoplot()

# lets remove the trend from the random walks

# 7 days moving average simulation 

diff(ts(y)) %>% ggAcf()
y[1:3,2] = 0
y[996:1000,2] = 0
ma = data.frame()
for ( i in seq(4,996)){
 if (i <= 1000){
   
   y[i,2] = (y[i-3,1] + y[i-2,1] + y[i-1,1] + y[i,1] + y[i+1,1]  + y[i+2,1] +  y[i + 3,1])/7
 }else{
   break
 } 
  
  
  
  
}

ts(y[,2]) %>% autoplot(series = "Data") + autolayer(ts(y[,1]), series = "7 Ma")  + scale_colour_manual(values = c("black","red"), breaks = c("Data","7 Ma")
)

eurousd<- read.csv(list.files()[[2]], header = FALSE)
names(eurousd)<- c("date","time", "open","high","low","close","volume")
eurousd<- eurousd %>% select(-time)
eurousd$date<- as.Date(eurousd$date,format = "%Y.%m.%d")


#=========================================

#for time series

eurousd_ts<- eurousd %>% select(date, close)
final_ts<- msts(eurousd_ts,seasonal.periods = c(7))[,2]
final_ts %>% autoplot()
final_ts  %>%  mstl() %>% autoplot()

eurousd_test<- eurousd_ts %>% filter(date >= "2019-07-08")
eurousd_test_ts<- msts(eurousd_test,seasonal.periods = c(7))
eurousd_train<-eurousd_ts %>% filter(date < '2019-07-08')
eurousd_train_ts<- msts(eurousd_train,seasonal.periods = c(7))

library(MLmetrics)
library(Metrics)

eurousd_train_ts[,2] %>% stlf(h = 5, lambda = 0) %>% autoplot() 

forecats<-eurousd_train_ts[,2] %>% stlf( method = "naive",h = 5)



fr_df<- melt(forecast(model,h=5))
fr_df$value.Point.Forecast<- round(fr_df$value.Point.Forecast,5)

pred_df<- data.frame(predicted = fr_df$value.Point.Forecast, actuals = eurousd_test$close)

RMSE(pred_df$predicted,pred_df$actuals)
MAE(pred_df$predicted,pred_df$actuals)

pred_df$predicted - pred_df$actuals





forecats$residuals %>%  checkresiduals()


#moving average simulation
vals<- rnorm(10000)
temp<- NULL

for(i in 3:10000){
  
  temp[i]<- vals[i] +  0.5 * vals[i-1] + 0.5* vals[i-2]
}

moving_1<- temp[3:10000]
moving_2<- ts(moving_1)
(acf(moving_2))


# autoregressie AR(1) process simulation

set.seed(545)
z = rnorm(1000)
x= NULL
phi = 0.6
phi_2= 0.2
x[1] = z[1]
x[2] = z[2]
for (t in 3:1000){
  
  x[t] = z[t] + phi * x[t-1] + phi_2 * x[t-2]
}

x_ts = ts(x)
autoplot(x_ts)

x_ts %>% ggtsdisplay()

set.seed(545)
arima.sim(list(ar = c(0.2,0.2,0.1,0.1,0.2)),n = 1000) %>% ts() %>% ggtsdisplay()



eurousd_train_ts[,2] %>% ggtsdisplay()
model<- eurousd_train_ts[,2] %>% Arima(order = c(1,1,0))
model %>% checkresiduals()

model_auto<- eurousd_train_ts[,2] %>% auto.arima()

model_auto %>% checkresiduals()


set.seed(2017)
arima.sim(list(ar = c(1/3,1/2)),n = 10000,sd = 4) %>% ggtsdisplay()

set.seed(2017)
t<-arima.sim(list(ar = c(1/3,1/2)),n = 2000000,sd = 4)

tacf = (acf(t))

r = tacf$acf[2:3]
R=matrix(1,2,2)
R[1,2] = r[1]
R[2,1] = r[2]

b = matrix(r,2,1)
solve(R,b)
#first one is estimate for phi and second one is estimate for phi2

set.seed(2017)
t<-arima.sim(list(ar = c(1/2)),n = 2000000,sd = 4)


R = matrix(1,3,3)
b =c(0.8,0.6,0.2)
R[1,2] = 0.8
R[1,3] = 0.6
R[2,1] = 0.8
R[2,3] = 0.8
R[3,1] = 0.6
R[3,2] = 0.8

solve(R,b)
5*(1-sum(solve(R,b) * b ))


rec %>% ggtsdisplay()

BoxCox(jj,lambda = 0.154072) %>% diff(1) %>% ggtsdisplay()

diff(log(jj)) %>% ggtsdisplay()

# ARMA simulations 

#Xt = 0.7*Xt-1 + Zt+0.2*Zt-1
# it is a combo of AR(1) and MA(1)
p_order = c(0,1,2,3)
q_order =c(0,1,2,3)

AIC_models<- data.frame()

for ( p in p_order){
  for (q in q_order){
    
    temp_model<- Arima(discoveries,order = c(p,0,q))
    temp_df<-data.frame(AIC = temp_model$aic,qOrder = q,pOrder = p)
    AIC_models<- rbind(AIC_models,temp_df)
    
  }
  
}

mod<-Arima(discoveries,order =c(1,0,1))
mod %>% checkresiduals()


#==============
births<-read_xlsx("birth.xlsx",sheet =1)
births<- births %>% rename(female_births = `Daily total female births in California, 1959`)
ts_birth<- ts(births$female_births)


ts_birth %>% autoplot()
#checking correlation of all ato correlation of the time series
Box.test(ts_birth, lag = log(length(ts_birth)))

# differintiating 

diff(ts_birth) %>% autoplot()
ts_birth %>% diff() %>% ggtsdisplay()


p_order = c(0,1,2,3,4,5,6,7)
q_order =c(0,1,2,3,4,5,6,7)

AIC_models<- data.frame()

for ( p in p_order){
  for (q in q_order){
    
    temp_model<- Arima(ts_birth,order = c(p,1,q), method = "ML")
    temp_df<-data.frame(AIC = temp_model$aic,qOrder = q,pOrder = p)
    AIC_models<- rbind(AIC_models,temp_df)
    
  }
  
}

AIC_models %>% arrange((AIC))


final_model <- Arima(ts_birth, order = c(4,1,3),method = "ML")

Box.test(final_model$residuals,lag = 10)
final_model %>% checkresiduals()


#==========



p_order = c(0,1,2,3,4)
q_order =c(0,1,2,3,4)

AIC_models<- data.frame()

for ( p in p_order){
  for (q in q_order){
    
    temp_model<- Arima(BJsales,order = c(p,2,q), method = "ML")
    temp_df<-data.frame(AIC = temp_model$aic,pOrder = p,qOrder = q)
    AIC_models<- rbind(AIC_models,temp_df)
    
  }
  
}

AIC_models %>% arrange((AIC))


final_model <- Arima(BJsales, order = c(1,2,2),method = "ML")

Box.test(final_model$residuals,lag = log(length(BJsales)))
final_model %>% checkresiduals()

plot(final_model)


#======================

eurousd_ts<- ts(eurousd$close)
eurousd_ts %>% autoplot()
eurousd_ts %>% diff() %>% autoplot() 
eurousd_ts_lamb<- BoxCox(eurousd_ts,lambda =  BoxCox.lambda(eurousd_ts))
eurousd_ts_lamb %>% diff(differences = 1) %>% autoplot()                         

eurousd_ts_lamb %>% diff(differences = 1) %>% ggtsdisplay()
Box.test(diff(eurousd_ts_lamb),lag = log(length(eurousd_ts_lamb)))

#=========================

p_order = c(0,1,2)
q_order =c(0,1,2)
P_order = c(0,1,2)
Q_order = c(0,1,2)

AIC_models<- data.frame()

for ( p in p_order){
  for (q in q_order){
    for(P in P_order){
      for(Q in Q_order){
      
    
  
    temp_model<- Arima(final_ts,order = c(p,1,q), seasonal = c(P,1,Q), method = "ML")
    temp_df<-data.frame(AIC = temp_model$aic,pOrder = p,qOrder = q,Porder = P,Qorder = Q)
    AIC_models<- rbind(AIC_models,temp_df)
    
  }
  
 
      }
  
}
}
AIC_models %>% arrange((AIC))


final_model<- Arima(final_ts,order=c(0,1,0), seasonal = c(0,1,1), method = "ML")
final_model %>% checkresiduals()

(final_model$residuals)^2 %>% tsdisplay()

# garch model

g.model<-garch(final_model$residuals,trace = FALSE)
g.model %>% checkresiduals()
g.model$residuals^2 %>% tsdisplay()

spec = ugarchspec(variance.model = list(garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(0,0)),distribution.model = "sged")
fit<- ugarchfit(spec,diff(final_ts),solver = "hybrid")

#=================================
# airpassengers forecasting with explonential smoothing 



