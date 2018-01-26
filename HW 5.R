####### HW #5
### 4.1 a)
x1 = 2*cos(2*pi*1:128*6/100) + 3*sin(2*pi*1:128*6/100)
x2 = 4*cos(2*pi*1:128*10/100) + 5*sin(2*pi*1:128*10/100)
x3 = 6*cos(2*pi*1:128*40/100) + 7*sin(2*pi*1:128*40/100)
x = x1 + x2 + x3
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-10,10), main=expression(omega==6/100~~~A^2==13))
plot.ts(x2, ylim=c(-10,10), main=expression(omega==10/100~~~A^2==41))
plot.ts(x3, ylim=c(-10,10), main=expression(omega==40/100~~~A^2==85))
plot.ts(x, ylim=c(-16,16), main="sum")

# b)
par(mfrow = c(1,1))
P <- abs(2*fft(x)/128)^2; Fr <- 0:127/128
plot(Fr,P,type="o",xlab="Frequency",ylab="Periodogram")

# c)
# Time series adding white noise
x1 <- 2*cos(2*pi*1:100*6/100)+3*sin(2*pi*1:100*6/100)
x2 <- 4*cos(2*pi*1:100*10/100)+5*sin(2*pi*1:100*10/100)
x3 <- 6*cos(2*pi*1:100*40/100)+7*sin(2*pi*1:100*40/100)
w <- rnorm(100,0,25)
x <- x1 + x2 + x3 + w
plot.ts(x,main="sum")
# periodogram
P <- abs(2*fft(x)/100)^2; Fr <- 0:99/100
plot(Fr,P,type="o",xlab="Frequency",ylab="Periodogram")

## 4.8
library(astsa)
# to plot the perdiogram
sp <- spec.pgram(sunspotz,taper=0,log="no")

# we can get a reverse sorted list of the specs 
sort_specs <- sort(sp$spec, decreasing = TRUE)[c(1,2,3)] # get top 3
#to get the higest spectrum freq peak
#the corresponding frequency
p1 <- sp$freq[sp$spec==sort_specs[1]]
p1

# freq for 2nd highest peak
p2 <- sp$freq[sp$spec==sort_specs[2]]
p2

# freq for 3rd highest peak
p3 <- sp$freq[sp$spec==sort_specs[3]]
p3

# when do the cycles occur
cat("cycles are occuring at", 1/p1, 1/p2, 1/p3, "years")

## Creating CI for ech of these peaks
make_CI <- function(peak_spec){
  u <- qchisq(0.025,2)
  l <- qchisq(0.975,2)
  c((2*peak_spec)/l,(2*peak_spec)/u)  
}

# for peak 1
make_CI(sort_specs[1])

# for peak 2 
make_CI(sort_specs[2])

# for peak 3 
make_CI(sort_specs[3])

## 4.21
om <- seq(0,5,0.0001)
power.spec <- 70+112*cos(2*pi*om)+56*cos(4*pi*om)+16*cos(6*pi*om)+2*cos(8*pi*om)
plot(om,power.spec,xlab="frequency",type="l")

