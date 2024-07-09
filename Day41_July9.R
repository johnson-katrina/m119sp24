rm(list=ls())

#Code from Day 38 (July 1)
f0 <- function(x,a=0,b=1){1/(b-a)+0*x}
f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}
f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}
f3 <- function(x,lambda=1){lambda*exp(-lambda*x)}

#Code from Day 39 (July 2)
library(data4soils)
Ng <- cfbp_fpjuliet$ng

  #f1 fit
  mu <- mean(Ng)
  sigma <- sqrt(var(Ng))
  mu
  sigma
x1 <- seq(-20,20,0.1)
y1 <- f1(x1,m=mu,s=sigma)
par(mfrow=c(1,1),mar=c(2.5,4,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Normal",xlim=c(-5,12))
lines(x1,y1,col=2)
  
  #f2 fit... Code from Day 39 (July 2)
  alpha <- mean(Ng)^2/var(Ng)
  beta <- mean(Ng)/var(Ng)
  alpha
  beta
x2 <- seq(0,20,0.1)
y2 <- f2(x2,a=alpha,b=beta)
par(mfrow=c(1,1),mar=c(2.5,4,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Gamma")
lines(x2,y2,col=2)



