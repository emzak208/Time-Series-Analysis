#### HW 4 
## 3.31
library(astsa)
plot(gnp)
acf(gnp,lag.max=55)
gnpgr=diff(log(gnp),1)
plot(gnpgr)
acf2(gnpgr)
sarima(gnpgr,1,0,0) # fit AR(1)

## 3.32
plot(oil)
acf(oil)
pacf(oil)
acf2(oil)
# transformaiton
oil_d <- diff(oil)
plot(oil_d)
oil_dl <- diff(log(oil), 1)
plot(oil_dl)
# look at acf and pacf
par(mfrow=c(2,1))
acf(oil_dl,lag.max = 300)
pacf(oil_dl,lag.max = 300)
# fit ARIMA (1,1,1)
fit1<- sarima(oil_dl,1,0,1)
fit1
fit2<- sarima(oil_dl,1,0,1)
fit2

## 3.33
plot(gtemp)
acf(gtemp)
# transformation
gtemp_d<- diff(gtemp)
plot(gtemp_d)
acf(gtemp_d)
# look at acf and pacf
par(mfrow=c(2,1))
acf(gtemp_d)
pacf(gtemp_d)
# fit ARIMA(0,1,2)
temp.fit<- sarima(gtemp,0,1,2)
temp.fit
# forecast for 10 years
pr<-sarima.for(gtemp,10,0,1,2)
pr$pred

## 3.36: Plot (or sketch) the ACF of the seasonal 
#ARIMA(0, 1) (1, 0)12 model with phi = .8 and theta = .5.

# fit model
phi = c(rep(0,11),0.8)
ACF = ARMAacf(ar=phi, ma=0.5,50)
plot(ACF,type="h", xlab="lag", ylim=c(-0.1,0.8))
abline(h=0)

## 3.37
plot(unemp)
acf(unemp)
# transform
par(mfrow=c(2,1))
unemp_d<-diff(unemp)
plot(unemp_d)

# acf and pacf 
par(mfrow=c(2,1))
acf(unemp_d)
pacf(unemp_d)
# fit model
# fit SARIMA(1,1,0,1,1,0)
unemp.fit1<- sarima(unemp,1,1,0,1,1,0,12)
unemp.fit1
# fit SARIMA(1,1,0,3,1,0)
unemp.fit2<- sarima(unemp,1,1,0,2,1,0,12)
unemp.fit2
# fit SARIMA(1,1,0,1,1,2)
unemp.fit3<- sarima(unemp,1,1,0,3,1,0,12) # the best
unemp.fit3
# make prediction for 12 months
unemp.pr<- sarima.for(unemp,12,1,1,0,3,1,0,12)
unemp.pr

###################### 3.37 Redo ############################## 
par(mfrow=c(1,1))
plot(unemp)
acf(unemp)
# transform
unemp_d1<- diff(diff(unemp),12)
plot(unemp_d1)
# acf and pacf 
par(mfrow=c(2,1))
acf(unemp_d1,lag.max = 48)
pacf(unemp_d1,lag.max = 48)
# fit model
# fit SARIMA(1,1,0,1,1,0)
unemp.fit1<- sarima(unemp,2,1,0,0,1,1,12)
unemp.fit1
# fit SARIMA(1,1,0,3,1,0)  # the best 
unemp.fit2<- sarima(unemp,2,1,0,2,1,1,12)
unemp.fit2
# make prediction for 12 months
unemp.pr<- sarima.for(unemp,12,2,1,0,2,1,1,12)
unemp.pr


