### HW #2
## Problem 2.1
# a)
library(astsa)
data('jj')
trend = time(jj) - 1970 # to center time
quarter=factor(rep(1:4,21)) # make quarter factor 
par(mfrow=c(2,1))
plot(jj,xlab='Time',ylab='Earning per share')
plot(log(jj))
fit=lm(log(jj)~0+trend+quarter,na.action = NULL) # fit the model,note: no intercept
summary(fit)
# From summary, beta,quarter 1,2,3,4 are all significant.

# b)
fit2=lm(log(jj)~trend+quarter,na.action=NULL)
summary(fit2)
# From the summary, the baseline is quarter 1. Quarter 2 is not signifiacnt, intercept, trend,quarter 3 and 4 are all significant. 

# c)
par(mfrow=c(1,1))
plot(log(jj))
lines(fitted(fit),col='red')
acf(residuals(fit))

## Problem 2.3
set.seed(2)
par(mfcol=c(3,2))
for(i in 1:6){
  x=ts(cumsum(rnorm(100,0.01,1)))
  fit=lm(x~0+time(x),na.action=NULL)
  plot(x)
  lines(0.01*time(x),col='red',lty='dashed')
  abline(fit,col='blue')
  }

## Problem 2.11
# a)
library(astsa)
plot(oil)
lines(gas,col='red',ylim=c(300))
plot(gas)

# c)
plot( diff(log(oil), differences=1))
lines(diff(log(gas)),col='red')
acf(diff(log(oil)))
acf(diff(log(gas)))

# d)
# ccf(diff(log(oil)), diff(log(gas)))
ccf(diff(log(gas)), diff(log(oil)))
# at lag 0, there's significant correlation 
# at lag -0.03 nad -0.06, pgas leads poil

# e)
lag2.plot(diff(log(oil)), diff(log(gas)),max.lag=3,cor=TRUE,smooth=TRUE)

# f)
# ii)
pgas<-diff(log(gas))
poil<-diff(log(oil))
indi = ifelse(poil < 0, 0, 1)
mess = ts.intersect(pgas, poil, poilL = lag(poil,-1), indi)
summary(fit <- lm(pgas~ poil + poilL + indi, data=mess))
# when oil negative, I=0
summary(fit <- lm(pgas~ 0+poil + poilL, data=mess))
# when oil positive/ no, I=1
summary(fit <- lm(pgas~ 0+poil + poilL + indi, data=mess))

# iii)
acf(resid(fit))
pacf(resid(fit))



