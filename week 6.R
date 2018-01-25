### week 6
# example check causality
#find roots for polynomial function
abs(polyroot(c(1,-0.75,0.5625))) # abs find the aboslute value
# example: Simulation for an Arima model
set.seed(4866)
ts.sim=arima.sim(list(order=c(1,1,0),ar=0.8),n=200) # simulate arima
plot(ts.sim)
# not stationary from plot
dif=diff(ts.sim,1) # detrend by taking first order difference 
plot(dif)
wh=arima(dif,order=c(1,0,0)) # fit arima model for difference 
acf(wh$res) # acf of residual , very good
pacf(wh$res)  # very good
# Conclusio : Arima (1,1,0) will do a good job in fitting the data

# 0.7985 this is the estimate for fi
# 0.918 estimate of white noise variance term, if not specified would be 1 
ar.yw(dif,order=1) # Yule-Walker estimate:specific for AR model 
wh

## another way of fitting arima model
library(forecast)
require(forecast)
auto.arima(ts.sim) # estimate of fi and sigma square

### Example Recruitment 
library(astsa)
plot(rec)
# not overall trend which means no strong mean, so might be stationary process
# diference is getting rid of mean function(trend)
rec.yw=ar.yw(rec,order=2) #  yule-walker estimate for AR(2)
rec.yw$x.mean # raw mean
rec.yw$ar # fi 1 and fi 2 estimates respectedly
sqrt(diag(rec.yw$asy.var.coef)) # adding normality, coef and variance, this tells the significant of AR(2) model
# since all the numbers are significantly from 0, it shows AR(2) models might work
acf(rec)
pacf(rec) # pacf is dying slowly, might be good explanation for AR(2) model,cuz only significant for lag 1 and 2

## prediction error
rec.yw$var.pred # or
rec.pr=predict(rec.yw, n.ahead=24) # predict 24 months ahead 
rec.pr

U=rec.pr$pred + rec.pr$se # upper limit for prediction interval
L=rec.pr$pred - rec.pr$se # lower limit for prediction interval
month=360:453 
plot(month,rec[month],type='o',xlim=c(1980,1990),ylab='recrius')
lines(rec.pr$pred,col='red', type='o')
lines(U,col='blue',lyt='dashed')
lines(L,col='blue',lyt='dashed')




