### Stat6555 HW #3
## 3.8
h <- 1:50

arma_11 <- function(theta, phi, h) {
  ((1 + theta * phi)*(theta + phi))/(1 + 2*theta*phi + theta^2) * phi^(h-1)}

ar_1 <- function(theta, h) {theta^}

ma_1 <- function(theta, h) {theta/(1 + theta^2)}

par(mfrow = c(3,1))

plot(h, arma_11(.9, .6, h), type = "l")
plot(h, ar_1(.9, h), type = "l")
plot(h[2:length(h)], ar_1(.9, h[2:length(h)]), type = "l")

## 3.9
ar_sim <-arima.sim(list(order=c(1,0,0),ar=.6),n=100)
ma_sim<-arima.sim(list(order=c(0,0,1),ma=.9),n=100)
ARMA <-arima.sim(list(order=c(1,0,1),ar=.6,ma=.9),n=100)

#plots AR(1)
par(mfcol=c(1,2))
acf(ar_sim)
pacf(ar_sim)

# plots MA(1)
par(mfcol=c(1,2))
acf(ma_sim)
pacf(ma_sim)

# plots ARMA(1,1)
par(mfcol=c(1,2))
acf(ARMA)
pacf(ARMA)

# compute sample ACF and PACF
ARMA = arima.sim(n=100, list(order=c(1,0,1), ma=.9, ar=0.6)) #ARMA(1,1)
AR = arima.sim(n=100, list(order=c(1,0,0) , ar=0.6)) # ARMA(1,0) = AR(1)
MA = arima.sim(n=100, list(order=c(0,0,1), ma=.9)) # ARMA(0,1) = MA(1)
par(mfrow=c(2,3))
acf(AR, lag.max = 20)
acf(MA, lag.max = 20)
acf(ARMA, lag.max = 20)
pacf(AR, lag.max = 20)
pacf(MA, lag.max = 20)
pacf(ARMA, lag.max = 20)

## 3.10
par(mfrow = c(1,1))
library(astsa)
library(forecast)

# Fit model
model_fit <- ar.ols(cmort)
model_fit

# check some fit performance 
par(mfrow=c(2,1))
acf(model_fit$resid, na.action = na.pass)
pacf(model_fit$resid, na.action = na.pass)
par(mfrow=c(1,1))
# to forecast the four period in the future

forecast_preds <- forecast.ar(model_fit, h = 4)
plot(forecast_preds)





