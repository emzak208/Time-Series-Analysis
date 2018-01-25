### week 5 ------------------------
##example AR(2) with 0.5 and 0.2
ar2=arima.sim(list(order=c(2,0,0),ar=c(0.5,0.2)),200) # simulate AR model
plot(ar2)
acf(ar2) # decaying slowly
pacf(ar2) # Significant at lag 1 and 2, other not sig.
# in theory, after lag 2, all values are 0.

## Example AR(1) ,0.9 and -0.9
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0),ar=0.9),n=100), main=expression(AR(1)~~phi==0.9)) # fit AR(1) 0.9
plot(arima.sim(list(order=c(1,0,0),ar=-0.9),n=100), main=expression(AR(1)~~phi==-0.9)) # fit AR(1) -0.9
# first graph positively correlated, so less up and downs
# Second gragh negatively corrlatedm so more up and downs
acf(arima.sim(list(order=c(1,0,0),ar=0.9),n=100)) # decay slowly
pacf(arima.sim(list(order=c(1,0,0),ar=0.9),n=100)) # pacf has significant lag only up tp pth lag
#-0.9
acf(arima.sim(list(order=c(1,0,0),ar=-0.9),n=100))
pacf(arima.sim(list(order=c(1,0,0),ar=-0.9),n=100)) # very significant negative relationship at lag 0


