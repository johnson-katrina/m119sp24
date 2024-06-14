rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
#y0 <- f2(x,a0=100,a1=0,a2=0)

y1 <- f2(x,a0=100,a1=7e-4,a2=0)
best.a1 <- sum(t*(y-100))/sum(t^2)
D2l5 <- -sum(t^2)
y1mle <- f2(x,a0=100,a1=best.a1,a2=0)

y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum(t*(y-100))
b.2 <- sum(t^2*(y-100))

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c.11, c.12, b.1, c.12, c.22, b.2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 
best.a1
best.a2

D <- (-c.11)*(-c.22) - (-c.12)^2
D
lxx <- -c.11
lxx
y2mle <- f2(x,a0=100,a1=best.a1,a2=best.a2)

#y3 <- f3(x,a1=-1.9,a2=0.00114)

y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
c.11 <- sum(t^2)
c.12 <- sum(t*log(0.005*t+1))
c.22 <- sum((log(0.005*t+1))^2)
b.1 <- sum(t*(y-100))
b.2 <- sum((log(0.005*t+1))*(y-100))

sol <- solvesystem(c.11, c.12, b.1, c.12, c.22, b.2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 
best.a1
best.a2

D <- (-c.11)*(-c.22) - (-c.12)^2
D
lxx <- -c.11
lxx
y4mle <- f4(x,a0=100,a1=best.a1,a2=best.a2)

y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)
best.a1 <- sum(t*exp(-0.00005*t)*(y - 100*exp(-0.00005*t)))/sum((t*exp(-0.00005*t))^2)
D2l5 <- -sum((t*exp(-0.00005*t))^2)
y5mle <- f5(x,a0=100,a1=best.a1,a2=0.00005)

# par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
# plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
# lines(x,y0,col=2)
# plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
# lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
lines(x,y1mle,col=4)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)
lines(x,y1mle,col=4)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
lines(x,y2mle,col=4)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)
lines(x,y2mle,col=4)

# par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
# plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
# lines(x,y3,col=2)
# plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
# lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
lines(x,y4mle,col=4)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)
lines(x,y4mle,col=4)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
lines(x,y5mle,col=4)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)
lines(x,y5mle,col=4)