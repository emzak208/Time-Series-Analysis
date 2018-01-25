## Example Recruitment
library(astsa)
rec.mle = ar.mle(rec, order=2) # using maximum likelyhood method to estimat fi1 and fi2
rec.mle
rec.mle$x.mean # overall mean
rec.mle$ar # coefficients : sig diff from 0
sqrt(diag(rec.mle$asy.var.coef)) # estimate standard error
# coefficient significantly diff from 0 is what we expect
rec.mle$var.pred # get prediction error

# fit in an Arima(p,d,q) --->  arima(data,order=(p,d,q))
rec.arima = arima(rec, order=c(2,0,0))
rec.arima
rec.yw = ar.yw(rec, order=2)# yule-walker method for AR process
rec.yw
acf(rec.yw$resid, na.action=na.pass) # looks good
pacf(rec.yw$resid, na.action=na.pass) # besides seasonal components, things look good

## Example Federal Reserve Board Production (prodn)
## ARIMA(1,1,1)*(2,1,1)12
plot(prodn)
plot(diff(prodn)) # try first order difference make non-stationay to stationary
# stationary: no trend depends on time
acf(diff(prodn))  # has seasonal components, SARIMA could be used
pacf(diff(prodn)) # seasonal part dying faster than non-seasonal part
# or 
fed = log(prodn) # take log get rid of trend then diff
plot(diff(fed)) # better than not taking log
acf(diff(fed))
pacf(diff(fed))
fed.fit = arima(fed, order =c(1,1,1), seasonal=list(order=c(2,1,1), period=12))
fed.fit # fit ARIMA with seasonal components
# all coefficients are significant

# get Diagnostics
tsdiag(fed.fit, fog.lag=48)
acf(fed.fit$res)
pacf(fed.fit$res) 
fed.pr = predict(fed.fit, n.ahead=24) # get prediction for 24 months later
fed.pr$pred 

# Another faster way of doing this
fed.fit2 = sarima(fed,1,1,1,2,1,1,12)
fed.fit2$fit # same coefficient numbers as in ARIMA
fed.pr2 = sarima.for(fed,24,1,1,1,2,1,1,12) # Predict for 2 years
fed.pr2$pred 
# ACF and PACF of residuals look very good, and qq-plot very good.
# or to show prediction and prediction intervals on graph:
sarima.for(fed,24,1,1,1,2,1,1,12)$pred

## Example : Australian Beer
library(fpp)
plot(ausbeer) # non-stationary,there is a trend 
seasonplot(ausbeer)
beer.fit = sarima(ausbeer, 1,1,1,1,1,0,4) # fit model before log
sarima.for(ausbeer, 8, 1,1,1,1,1,0,4)$pred # predict for 8 months

# after taking log
lbeer = log(ausbeer) # no difference after log transformation
plot(lbeer)
beer.fit = sarima(lbeer, 1,1,1,1,1,0,4) # fit ARIMA(1,1,1)*(1,1,0)4; 4 means 4 quarters
# ACF result very good, qq-plot very good
names(beer.fit) # check what results are in 'fit' 
sarima.for(lbeer, 8,1,1,1,1,1,0,4)$pred # predict for 8 quarters
# this will show prediction on graph
# or to get prediction values:
pr3=sarima.for(lbeer, 8,1,1,1,1,1,0,4) # will give prediction values
pr3$pred 






