## week 3 4/10: sample coreelation functions
library(astsa)
lag.plot(soi,lags=12,diag=F) # relationships are week linear: current time point is not positively related to previous time point 
# lag 1 clear relationship, the others not.

acf(soi,type='covariance')
acf(soi)

# eg 1
acf(rec) # does not look like white noise,cuz mean does not look liek zero

## white noise should look like this (two standard deviation from the mean should contain 95%)
w=rnorm(500)
acf(w,lag.max = 40)
# the first high line is the autocoreeltion between time t with time t, which is 1 by definition

#eg 2 : moving average with 12 and 102 samples
# moving average: 1/3(wt-1+wt+wt-1)
set.seed(4866)  
w1=rnorm(12) # generate 12 randomly
w2=rnorm(102) # generate 102 randomly
v1=filter(w1,sides=2,rep(1/3,3))[-c(1,12)]  # sides: past and future time poits for moving average
v2=filter(w2,sides=2,rep(1/3,3))[-c(1,102)] # remove 1st the 102th elements
par(mfrow=c(2,1))
acf(v1,lag.max = 4)
acf(v2,lag.max = 4)
# blue lines are the standard error:cross-correlation function
# with blue line: cross-covariance function
acf(v1,lag.max = 4,plot = FALSE) # get numeric valuse instead of plots
acf(v2,lag.max = 4,plot=FALSE)
# v2 dies faster than v1

# problem Compare with the calculation we did in class
par(mfrow=c(1,1))
set.seed(4866)
w1=rnorm(500)
v1=filter(w1,sides = 1,filter=c(0.9,-0.1))[-1]
acf(v1)
acf(v1,plot = FALSE,lag.max = 4)
# acf looks good


