#HW 1
#1.1
library(astsa)
x11()
ts.plot(EQ5,EXP6,gpars= list(col=rainbow(2)))
legend("topleft", legend = 1:2, col = 1:2, lty = 1)
#lines(EQ5,lty="dashed")     add line)

# 1.2
#a)
w=rnorm(200,0,1)
s = c(rep(0,100), 10*exp(-(1:100)/20)*cos(2*pi*1:100/4))
#x=s+w   <-------------wrong ,because has to put time series
x=ts(s+w)
plot(x)

#b)
w=rnorm(200,0,1)
s = c(rep(0,100), 10*exp(-(1:100)/200)*cos(2*pi*1:100/4))
#x=s+w   <-------------wrong ,because has to put time series
x=ts(s+w)
plot(x)

#c)
par(mfrow=c(2,1))
a<-exp(-(1:100)/20)
plot(a)
b<-exp(-(1:100)/200)
plot(b)

#1.7    
# solution online 
x11()    
x_ma <- numeric(499-2)  #initialize sigma=0
w <- rnorm(500)
for (i in 2:499) {x_ma[i] = w[i-1] + 2*w[i] + w[i+1]}
x_ma <- x_ma[25:length(x_ma)] # eliminate the cold start (i think)
acf(x_ma)


# my solution
par(mfrow=c(1,1))
xt<-numeric(499-2) 
w<-rnorm(500)
for (i in 2:499){xt[i]=w[i-1]+ 2*w[i]+ w[i+1]}
acf(xt, lag.max = 2,
    type = c("correlation"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

#1.22
w=rnorm(200,0,1)
s=c(rep(0,100),10*exp(-(1:100)/20)*cos(2*pi*1:100/4))
#x=s+w   <-------------wrong ,because has to put time series
x=ts(s+w)
plot(x)
acf(s,lag.max = 100)

#1.23
w<-rnorm(500,0,1)
x<-2*cos(2*pi*(1:100+15)/50)+w
acf(x,lag.max=100,type='covariance')



