#Code adapted from Day 8 (May 3)
rm(list=ls())
library(data4led)
dist <- led_time(2100)
par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(dist$percent_intensity,probability = TRUE)

f0 <- function(L,a=0,b=1){
  # Make sure a < b when using this function.
  ifelse(L < a,NaN, ifelse(L <= b, 1/(b-a), NaN))
}
f1 <- function(L,h=0,a=1){
  #Make sure h > 0 and a > 0.
  1/sqrt(2*pi*a)*exp(-(L-h)^2/(2*a))
}
f2 <- function(L,h=0,a=1,b=5){
  # Make sure a > 0 and b > 0.
  
  out <- rep(-1,length(L))
  out[(L < h)] <- 0*L[(L < h)]
  out[(L >= h)] <- b^a/gamma(a)*(L[(L >= h)]-h)^(a-1)*exp(-b*(L[(L >= h)]-h))
  
  return(out)
}


a <- 99.5
b <- 103.5
L <- seq(a,b,0.1)
y0 <- f0(L,a,b)
hist(dist$percent_intensity,probability = TRUE,xlim=c(95,105),ylim=c(0,1))
lines(L,y0,col=2)
#Use simulation to compute probability.
n <- 5000
bulbs <- runif(n,a,b)
x <- length(which(bulbs > 100.2533 & bulbs < 102.6927))
x
p <- x/n
p



a <- 0.5
L <- seq(80,120,0.1)
h <- 101.5
y1 <- f1(L,h,a)
hist(dist$percent_intensity,probability = TRUE,xlim=c(95,105),ylim=c(0,1))
lines(L,y1,col=2)
#Use simulation to compute probability.
n <- 5000
bulbs <- rnorm(n,h,a)
x <- length(which(bulbs > 100.2533 & bulbs < 102.6927))
x
p <- x/n
p



h <- 98.5
a <- 17
L <- seq(h,120,0.1)
b <- 6
y2 <- f2(L,h,a,b)
hist(dist$percent_intensity,probability = TRUE,xlim=c(95,105),ylim=c(0,1))
lines(L,y2,col=2)
#Use simulation to compute probability.
n <- 5000
bulbs <- rgamma(n,shape=a,rate=b)+h
x <- length(which(bulbs > 100.2533 & bulbs < 102.6927))
x
p <- x/n
p