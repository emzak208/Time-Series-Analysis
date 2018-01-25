### week 3 04/12/2017
## pacf: partial auto-correlation function
## in practice, we minimize SSE with estimated PACF
## For non-stationary: acf decays slowly while pacf decay quickly
library(astsa)

## Example SOI
pacf(soi) # the first month predict some linearity. but the data is not telling anything valuable two month ago related to the current value if added the first month data .
# the first line is the last month.(highly correlated)
# lag 1 is the first for PACF , which means lag 0 does not make sense


## Example REC
pacf(rec)

## Example gtemp: trend model=ts linear regression model
fit=lm (gtemp~poly(time(gtemp),4,raw=TRUE),na.action=NULL) # fit general polynomial model
summary(fit)
plot(gtemp)
# since all p value are significant, it is a concern, so we need to think about using a higher order polynomial
plot(resid(fit))  # or fit$resid # residuals looks like a constant 
acf(resid(fit))
pacf(resid(fit)) 
acf(gtemp) # compare this to acf of resids, acf of resids works better

## choose the smallest value of these,analagy to R^2
## AIC, BIC and AICC are all based on -2LogLikelihood
## maximum likelyhood estimate parameters
## but it's more convinient to work with -log(likelihood)
# AIC ?
AIC(fit)/length(gtemp)-log(2*pi) 
#BIC ? # large sample size 
AIC(fit, k= log(length(gtemp)))/length(gtemp)-log(2*pi)
#AICC ?
(AICc = log(sum(resid(fit)^2/length(gtemp))+(length(gtemp)+4)/(length(gtemp)-4.2))) # small sample size with many parameters

## Example(SOI): stationary with seasonal component
month  = rep(1:12, 38)[-c(454:456)]
dummies = table(1:length(month), as.factor(month)) # turn catagorial to dummy
summary(lm(soi~time(soi) + dummies[,1:11])) # ,dummies is the seasonal components within a regression model.can use any 11 in 12 monthes
plot(soi)

# cotinue on week 4