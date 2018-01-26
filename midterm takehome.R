###### midterm take homw
#### problem 1
### getFinancials('VMW',src = "yahoo")
library(quantmod)
getSymbols('VMW', from='2015-04-01', to='2017-04-01')
# plots 
plot(VMW[,4]) # plot close price vs time: not stationary, also has seasonal components
par(mfrow=c(2,1)) 
acf(VMW[,4]) #  decays slowly
pacf(VMW[,4]) # significant at lag one, and not significant after lag 1
# From ACF and PACF, it looks like AR(1)

### Transformation 
fed.VMW=log(VMW[,4])
after.tran1=diff(fed.VMW,difference=2)[3:505] # 2nd diff on log
after.tran2=diff(VMW[,4])[2:505] # first diff on original 
par(mfrow=c(3,1))
plot(fed.VMW) # not much difference 
plot(diff(VMW[,4])[2:505]) # try taking first order difference, detrend very good
plot(diff(fed.VMW)[2:505]) # tk difference on log, not much difference 
# 2nd order difference,best so far
# acf pacf plot 2nd order diff(log): looks like MA(1)
plot( diff(fed.VMW, differences=2)[3:505]) # 2nd order difference,best so far
acf(diff((fed.VMW),differences = 2)[3:505])  
pacf(diff((fed.VMW),difference=2)[3:505]) 
# acf pacf for first order diff: indicates MA(1)
acf(after.tran2)
pacf(after.tran2)


### Fitting model
## Fit SARIMA(1,0,1)
# coefficient for ar1 almost 0
fit1=sarima(after.tran1,1,0,1) # ACF looks like white noise
fit1$fit # normal plot good

# try SARIMA(0,0,1)
# residual looks good,normal plot looks good
# coefficient for ma1 significantly diff from 0, very good
fit2=sarima(after.tran1,0,0,1)
fit2$fit

# try SARIMA(1,0,1)on first diff of original
# ACF looks like white noise
# coefficient for ar1 and ma1 all significant, model fits
fit3=sarima(after.tran2,1,0,1) 
fit3$fit 

# try SARIMA(0,0,1)on first diff of original
# coefficient close to zero, not good
fit4=sarima(after.tran2,0,0,1) # ACF looks like white noise
fit4$fit

## overall, fit2 looks good. SARIMA(0,0,1)

### Making predictions
# make prediction for 20 days using fit2
fit2.pred = sarima.for(after.tran1,20,0,0,1) 
fit2.pred$pred 
sarima.for(after.tran1,20,0,0,1)$pred # why predict value is zero

# make prediction for 20 days using fit3
fit3.pred = sarima.for(after.tran2,20,1,0,1) 
fit3.pred$pred 
sarima.for(after.tran2,20,1,0,1)$pred





# or
## FIt ARIMA(1,0,1): result same as SARIMA
VMW.arima1= arima(after.tran1, order=c(1,0,1))
VMW.arima1 
VMW.arima2= arima(after.tran2, order=c(1,0,1)) # I used this one
VMW.arima2 # coeffeicient significantly different from zero, better than VMW.arima1
# get Diagnostics
tsdiag(VMW.arima2, fog.lag=48)
acf(VMW.arima2$res)
pacf(VMW.arima2$res) 
VMW.arima2.pr = predict(VMW.arima2, n.ahead=20) # get prediction for 20 days later
VMW.arima2.pr$pred 

# prediction interval
U=fit3.pred$pred + fit3.pred$se # upper limit for prediction interval at 95% significance
L=fit3.pred$pred - fit3.pred$se # lower limit for prediction interval at 95% significance
U
L

# plot prediction
plot(fit3.pred$pred,col='red',type='o')


# Detransform ?
detrans=diffinv(after.tran2) 
detrans
Yhat<-cumsum(c(VMW[,4],after.tran2)[1]) # works !!
detrans<-cumsum(c(VMW[,4],fit3.pred$pred)[1])
detrans

## plot of fitted to original ? 
library(forecast)
library(astsa)
plot(fit3.pred$pred,col='red',type='o')
ts.plot(after.tran2,fit3.pred$pred, col=1:2,xlim=c(1:525))
plot(fit3.pred$pred,col='red',type='o')
lines(U,col='blue',lyt='dashed')
lines(L,col='blue',lyt='dashed')

# plot fitted value on original data ?
plot(VMW[,4],type = 'l')
lines(diffinv(fit3.pred$pred),col='blue') # lines predicted fitted value on original

#### Problem 2
library(astsa)
data("AirPassengers")
air<-AirPassengers
air
plot(air,ylab='passengers in 1000s')

## transformation
# log: not very well
lair=log(air)
plot(lair)
# diff: not very good
dair=diff(air)[2:144]
plot(dair,type='l')
# diff on log : stationary
dlair=diff(lair)[2:144]
plot(dlair,type='l')

## fitting regression model with trend and seasonal component on transformed data
trend=time(air)
month=factor(rep(1:12,12)) 
fit=lm(dlair~trend[-1]+month[-1])
summary(fit) 

## step 3: look at residuals
acf(residuals(fit))
pacf(residuals(fit))
# plot fitted value on tup of transformed data
plot(dlair,type='l')
lines(fitted(fit),col='red') # overlay the plot
# plot fit
par(mfrow = c(2, 2))
plot(fit) # residual and normal plot not good
# nomality test
shapiro.test(fit$residuals) # Resisual normal
# equal variance: 
library(lmtest)
bptest(fit)

## step 4: predict the next year
pred=c(fitted(fit),0.2745069-0.0001495*c(2017.083,2017.167,2017.25,2017.33,2017.42,2017.50,2017.58,2017.67,2017.75,2017.83,2017.92,2018)+c(0,0.1580534,-0.0034301,0.0254786,0.1500099,0.1318185,0.0185938,-0.1167335,-0.1102447,-0.1157917,0.1418035,0.0429617))
pred[1:12]

# prediction interval
U=fit$fitted.values+1.96*fit$residuals
L=fit$fitted.values-1.96*fit$residuals
U[1:12]
L[1:12]

## prediction error
fit.pr= predict(fit, n.ahead=12) # same result as pred
fit.pr[1:12]
U=fit$pred + fit$se # upper limit for prediction interval
L=fit$pred - fit$se 
U
L

###################################################
library(astsa)
data("AirPassengers")
air<-AirPassengers
air
plot(air,ylab='passengers in 1000s')

## fitting regression model with trend and seasonal component on transformed data
trend=time(air)
month=factor(rep(1:12,12)) 
fit=lm(air~trend+month)
summary(fit) 

## step 3: look at residuals
acf(residuals(fit))
pacf(residuals(fit))
# plot fitted value on top of original data
ts.plot(air,fitted(fit),gpars=list(col=rainbow(2)))
legend('topleft',legend=1:2,col=1:2,lty=1) # overlay the plot

# plot fit
par(mfrow = c(2, 2))
plot(fit) # residual and normal plot not good
# nomality test
shapiro.test(fit$residuals) # Resisual normal
# equal variance: 
library(lmtest)
bptest(fit)

## step 4: predict the next year
pred=c(fitted(fit),0.2745069-0.0001495*c(1961,1961.083,1961.167,1961.25,1961.33,1961.42,1961.50,1961.58,1961.67,1961.75,1961.83,1961.92)+c(0,0.1580534,-0.0034301,0.0254786,0.1500099,0.1318185,0.0185938,-0.1167335,-0.1102447,-0.1157917,0.1418035,0.0429617))
predict(fit,interval='prediction')
# another way of predicting
fit.pr= predict(fit, n.ahead=12,intervals='prediction') # same result as pred
fit.pr[1:12]

# prediction interval
U=fit$fitted.values+1.96*fit$residuals
L=fit$fitted.values-1.96*fit$residuals
U[1:12]
L[1:12]

y<-cumsum(c(air,dlair)[1]) # works !!
detrans<-cumsum(c(y,fit$fitted.values)[1])
detrans

y




