### Time Series Take Home Final
### 06/05/2017

## Data preparation
library(forecast)
library(astsa)
mtf<-read.csv('monthly-traffic-fatalities-in-on.csv',header = TRUE)
colnames(mtf)<-c('month','monthly_traffic_fatalities_in_Ontario_1960_1974')
mtf<-mtf[-181,] # remove the last row 
mtf<-mtf[,'monthly_traffic_fatalities_in_Ontario_1960_1974']
mtf
# chaneg data to time sereis 
mtf.ts<-ts(mtf, start=c(1960, 1), end=c(1974, 12), frequency=12)
mtf.ts

## plots before transformation 
plot(mtf.ts,ylab='monthly traffic fatalities in Ontario 1960-1974') # not stationary with seasonla components
par(mfrow=c(1,2)) 
acf(mtf.ts) 
pacf(mtf.ts)

## Transformation :
lmtf<-log(mtf.ts) # take log
dmtf1<-diff(lmtf,difference=1)[2:180] # first order diff on log
dmtf2<-diff(lmtf,difference=2)[3:180] #  2nd order diff on log, looks good
dmtf3<-diff(diff(mtf.ts))[3:180] # 2nd order diff on original data, look good

## plots after transformation 
par(mfrow=c(3,1))
plot(lmtf) # log
plot(dmtf1,type='l') # 1st diff on log
plot(dmtf2,type='l') # 2nd diff on log : good 
plot(dmtf3,type='l') # 2nd diff on original : good 

## plot ACF and PACF
par(mfrow=c(2,2))
acf(dmtf2,na.action = na.pass)
pacf(dmtf2,na.action = na.pass)
acf(dmtf3,na.action = na.pass)
pacf(dmtf3,na.action = na.pass)
# 2nd diff on original looks the best

## fit model 
auto.arima(mtf.ts)
fit1<-sarima(mtf.ts,1,1,0,2,0,0,12) # residual does not look like white noise 
fit1$fit

fit2<-sarima(mtf.ts,2,1,0,2,0,0,12) # all coefficients significantly diff from zero,
fit2$fit                                   #  AIC 1620 ,better residual 


fit3<-sarima(mtf.ts,2,0,0,2,0,0,12) # AIC 1605
fit3$fit

fit11<-sarima(mtf.ts,2,1,0,2,1,0,12) # AIC 1500, residual no good
fit11$fit

fit12<-sarima(mtf.ts,2,1,0,2,1,1,12)  # AIC 1481, 
fit12$fit

fit13<-sarima(mtf.ts,2,1,1,2,1,1,12) # 1462, ACF the best, normal not very good
fit13$fit

fit14<-sarima(mtf.ts,2,1,1,2,1,0,12) # no good
fit14$fit

fit17<-sarima(mtf.ts,3,1,1,2,0,0,12) # error  
fit17$fit

# try ma=12, ma=13 cuz ACF cuts off after lag 12 or 13
fit18<-sarima(mtf.ts,2,1,12,2,0,0,12) # resuail good

fit19<-sarima(mtf.ts,2,1,13,2,0,0,12) # same as ma=12
fit19$fit
# but, coefficients not significantly diff from 0, no good!

fit20<-sarima(mtf.ts,2,1,3,2,1,1,12) # AIC 1466, 1960 straight line, normal not good
fit20$fit

fit21<-sarima(mtf.ts,2,1,3,2,1,1,12) # same as ma=12
fit21$fit

fit22<-sarima(mtf.ts,2,1,1,0,0,1,12) # residuals no good 
fit22$fit



#fit10<-sarima(mtf.ts,2,1,0,2,0,1,12)
fit10 <- arima(mtf.ts, order=c(2,1,0), seasonal=list(order=c(2,0,1)), 
             method="ML")
acf(fit10$residuals)
pacf(fit10$residuals)
# pacf not white noise 

## auto.arima on transformed data
# diff(log())
auto.arima(dmtf2)
fit4<-sarima(dmtf2,2,0,0)
fit4$fit

# 2nd order diff
auto.arima(dmtf3)
fit5<-sarima(dmtf3,2,0,0) # worst 
fit5$fit

##
fit6<-sarima(dmtf3,2,1,0,2,0,0,12) # residuals not good
fit6$fit

fit7<-sarima(dmtf3,1,1,0,2,0,0,12) # residuals not good
fit7$fit

##
fit8<-sarima(dmtf2,1,1,0,2,0,0,12) # residuals not good
fit8$fit

fit9<-sarima(dmtf2,2,1,0,2,0,0,12) # residuals not good
fit9$fit

fit16<-sarima(dmtf3,2,1,1,2,1,1,12) # residual 2 peaks, normal ok
fit16$fit

## Final model:
fit13<-sarima(mtf.ts,2,1,1,2,1,1,12) # 1462, ACF the best, normal not very good
fit13$fit

# Make predictions
par(mfrow=c(1,1))
pred<-sarima.for(mtf.ts,24,2,1,1,2,1,1,12)
pred

# prediction intervals:
U<-pred$pred+pred$se
U
L<-pred$pred-pred$se
L











