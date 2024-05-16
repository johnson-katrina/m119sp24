#Code from Day 12 (May 10)
rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity
plot(t,y)

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=0,a2=1){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f2(x,a0=100,a1=0,a2=0)
y1 <- f2(x,a0=100,a1=7e-4,a2=0)
y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
y3 <- f3(x,a1=-1.9,a2=0.00114)
y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)


#Use each function to answer the questions 
    # (1) What is the intensity of the bulb after 12000 hours?
    # (2) When is the intensity of the bulb 90% of its original intensity?

#f0
f2(12000,a0=100,a1=0,a2=0)
    #This fitted model f0 predicts will be at 100% of the original intensity after 12000 hours.
#The equation 100=90 has no solution. 
    #This fitted model f0 predicts that the bulb will never be at 90% of the original intensity.

#f1
f2(12000,a0=100,a1=7e-4,a2=0)
    #This fitted model f1 predicts will be at 108.4% of the original intensity after 12000 hours.
#The equation 100+0.0007t=90 with t>=0 has no solution. 
    #This fitted model f1 predicts that the bulb will never be at 90% of the original intensity.

#f2
f2(12000,a0=100,a1=1.1e-3,a2=-1.5e-7)
    #This fitted model f2 predicts will be at 91.6% of the original intensity after 12000 hours.
f2shift <- function(x){f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)-90}
uniroot(f2shift,c(0,20000))$root
    #This fitted model f2 predicts that the bulb will be at 90% of the original intensity after 12617.15 hours.


#f3
f3(12000,a1=-1.9,a2=0.00114)
    #This fitted model f3 predicts will be at 101.6% of the original intensity after 12000 hours.
#The equation 101.9 - 1.9*exp(-0.00114*t)=90 with t>=0 has no solution. 
    #This fitted model f3 predicts that the bulb will never be at 90% of the original intensity.


#f4
f4(12000,a0=100,a1=-1.81e-4,a2=0.83)
    #This fitted model f4 predicts will be at 101.24% of the original intensity after 12000 hours.
f4shift <- function(x){f4(x,a0=100,a1=-1.81e-4,a2=0.83)-90}
uniroot(f4shift,c(60000,100000))$root
    #This fitted model f4 predicts that the bulb will be at 90% of the original intensity after 82897.49 hours.


#f5
f5(12000,a0=100,a1=6.23e-3,a2=5.06e-5)
    #This fitted model f5 predicts will be at 95.22% of the original intensity after 12000 hours.
f5shift <- function(x){f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)-90}
uniroot(f5shift,c(0,20000))$root
    #This fitted model f5 predicts that the bulb will be at 90% of the original intensity after 15334.42 hours.