### week 4, 4/17/2017
## from previous class
## example(SOI)
# conclusion: whereever the black is high, the red is high too.
# From ACF plot, we can conclude that this will not fit a normal regression model, cuz if it looks like wihite noise, the first line is high and rest of them should be within blue line, which is not the case here
# The residuals is not look like white noise therefore we can not fit the regression model
# residuals for model you fitting should looks like white noise
# Moving avaregae is always stationary
par(mfrow=c(1,1))
month= rep(1:12, 38)[-c(454:456)]
dummies=table(1:length(month), as.factor(month))
library(astsa)
fit1 = lm(soi~time(soi)+dummies[,1:11])
summary(fit1)

require(MASS)
stepAIC(fit1, direction='both')
fit2 = lm(soi~ time(soi)+dummies[,c(2,4:10)]) # after removing insignificant months
summary(fit2)

plot(time(soi), fitted(fit2), col='red', type='l') # plot independent variable vs fitted values
lines(soi) # put 'soi' plot onto the previous plot
# plot(time(soi), soi, col='red', type='l') # plot independent variable vs dependent
# lines(fitted(fit2))
acf(resid(fit2)) # resids does not look like white noise, so model does not fit
plot(resid(fit2))
plot(resid(fit2), type='l')

## Example: Bike data: four years of quarterly maintain bike sales.
# Fit a time series regression model with seasonality
## step 1 , plot data
library(astsa)
bike_data<-read.csv("bike.txt")
quarter=rep(1:4,4) # convert the object to a time series object
bike=ts(c(10,31,43,16,11,33,45,17,13,34,48,19,15,37,51,21),frequency=4,start=c(2007,1))
plot(bike) # always the first step to visualize
# from data, bike sells highest at summer time
# the trend is slightly increase over time from year to year,within year we have seasonal components

## step 2 : fit the time series regression model
fit=lm(bike~time(bike)+factor(quarter))
summary(fit)
# from summary, quarter 1 is baseline, everything is significant compared to quarter 1.(have stars)

## step 3: look at residuals
acf(residuals(fit))
# except for the first one, all lines are within blue line which is good
# the blue line is so big cuz the sample size is only 16
pacf(residuals(fit))
# our model is doing a great job, residuals looks like white noise and all of them are significant
plot(time(bike),fitted(fit),type='1') # plot independent variable vs fitted value
lines(bike,col='red') # overlay the plot

## step 4: predict the next year of sales
pred=c(fitted(fit),-4004.75+2*c(2011,2011.25,2011.5,2011.75)+c(0,21,33.5,4.5))
# the last c is the coefficient
plot(c(time(bike),2011,2011.25,2011.5,2011.75),pred,type='l')
points(bike,type='o',col='red') # plot original bike sale on prediction plot
pred

##### Example(gtemp) regression vs difference
## detrend using first order difference; log can also detrend, but better for seasonal components
par(mfrow=c(2,1))
fit=lm(gtemp~time(gtemp),na.action=NULL) # fitting a regression model
plot(resid(fit))
# detrend using difference
plot(diff(gtemp))
par(mfrow=c(3,1))
acf(gtemp,48,main='gtemp') # ACf for original data
acf(resid(fit),48,main= 'detrended') # ACF for regression model residual
acf(diff(gtemp),48,main='difference') # ACF for first order difference residual
# first order diff already detrend the model
# From ACF for residuals, first order difference is the best for this example
plot(diff(gtemp))







