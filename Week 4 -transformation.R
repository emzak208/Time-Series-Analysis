### week five :Transformation 
plot(varve)
plot(log(varve))
library(MASS)
plot( diff(log(varve), differences=1)) # log takes care of peaks,diff takes care of trend
plot(diff(varve)) # diff already detrend
boxcox( (diff(varve)-min(diff(varve))+2)~time(varve)[2:634])
?boxcox
boxcox( (diff(varve)-min(diff(varve))+2)~time(varve)[2:634], plotit=FALSE)

## Box-cox transormation
# make series all positive by adding 2 to the smallest value(any constant is fine)
# boxcox(y~x)
# [2:634] : difference starts with n-1 time,2-1=1 --> starts with time 1
bc = boxcox( (diff(varve)-min(diff(varve))+2)~time(varve)[2:634])
bc$x[which(bc$y==max(bc$y))] # get the exact value of lambda
bc = boxcox( varve~time(varve))# boxcox before taking difference
bc$x[which(bc$y==max(bc$y))]
plot(varve^(-0.1010101)) # plot varve^lamnda
plot(diff(varve^(-0.1010101))) # first box-cox transform,then take difference  
plot((diff(varve)-min(diff(varve))+2)^1.272727) # first take difference, then boxcox
# * first do boxcox, the take difference give a better result

## Example : Mortality Rates using linear filters smoothing techniques
ma5 = filter(cmort, sides=2, rep(1,5)/5) # filtering using a=5
ma53 = filter(cmort, sides=2, rep(1,53)/53) # filtering using a=53
plot(cmort)
lines(ma53, col='red')
lines(ma5, col='blue')
# red line over-smoothing, not good
# Blue line is better
plot(ma5)
ma7 = filter(cmort, sides=2, rep(1,7)/7) # filtering using a=7
plot(ma7) # ma7 is pretty good
week = rep(1:52, 10)[-c(509:520)] # 52 weeks in a year,and remove the extra weeks created
fit7 = lm(ma7 ~ time(cmort) + as.factor(week)) # fit model for after smoothing: ma7
summary(fit7) # week 1 to 3, and week 506,507, 508 not significant
plot(time(cmort)[-c(1:3, 506:508)], fitted(fit7), type='l') # plot original time to fitted values removing insignificant values
lines(ma7, col='red') # ma 7 did a good job in smoothing

## other smoothing techniques:
# Kernal smoothing: ksmooth(x,y)
plot(cmort, type='p')
lines(ksmooth(time(cmort), cmort, 'normal', bandwidth=8/52)) # apply kernal smoothingy
plot(cmort, type='l') # plot original data with lines
lines(ksmooth(time(cmort), cmort, 'normal', bandwidth=8/52), col='red') # apply ksmooth on top

# supersmoother
s1 = supsmu(time(cmort), cmort, span=0.01) # apply supersmoother: supsmu(x,y)
plot(cmort, type='p') 
lines(s1)  # on top of original
plot(cmort-s1$y) 
acf(cmort-s1$y)

# lowess smoothing technique: scatter plot smoothing
plot(cmort, type='p') # plot original
lines(lowess(cmort, f=0.02)) # apply lowess smoothing 

# spline smoothing technique: smooth.spline()
plot(cmort, type='p') # plot original 
lines(smooth.spline(time(cmort),cmort)) 
