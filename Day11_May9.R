f <- function(x){5+3*exp(2*x)}
f.shift <- function(x){f(x)-7}
uniroot(f.shift,c(-10,10))
uniroot(f.shift,c(-10,10))$root

0.5*log(2/3)


library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)


f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,800001,2)
yM <- f5(x,a0=100,a1=0.0053,a2=0.0000433)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)


f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,80001,2)
yM <- f1(x,a0=101,a1=0.00025)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)