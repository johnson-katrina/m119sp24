#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2 <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Graph the functions.
x <- seq(-5,5,1e-3)

###figure 1###
y1 <- f1(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y1,type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(3-1,3+1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.75,3.25),ylim=c(-12.5,12.5))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.9,3.1),ylim=c(-5,5))
points(3,f1(3),pch=16,col=2)


###figure 2###
y2 <- f2(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y2,type='l',xlim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-1,1),ylim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.5,0.5),ylim=c(-2.5,2.5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.01,0.01),ylim=c(-.05,.05))
points(0,f2(0),pch=16,col=2)

###figure 3###
x3 <- seq(0,5,1e-3)
y3 <- f3(x3)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x3,y3,type='l',xlim=c(0,5), ylim=c(-2.5,2.5) )
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1,3),ylim=c(0,2))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.5,2.5),ylim=c(0.5,1.5))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)

###figure 4###
x4 <- seq(2,10,1e-3)
y4 <- f4(x4)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x4,y4,type='l',xlim=c(0,10), ylim = c(-5,5))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(1.75,3.75),ylim=c(f4(2.75)-1,f4(2.75)+1))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.5,3),ylim=c(f4(2.75)-0.25,f4(2.75)+0.25))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.7,2.8),ylim=c(f4(2.75)-0.05,f4(2.75)+0.05))
points(2.75,f4(2.75),pch=16,col=2)





#Define the linearization.
t <- function(x,a,fa,df){
  fa + df*(x - a)
}

#f1
plot(x,y1,type='l',xlim=c(2.9,3.1),ylim=c(-5,5))
points(3,f1(3),pch=16,col=2)
m1 <- 51
lines(x,t(x,3,f1(3),m1),col=3)

#f2
plot(x,y2,type='l',xlim=c(-0.01,0.01),ylim=c(-.05,.05))
points(0,f2(0),pch=16,col=2)
m2 <- 2
lines(x,t(x,0,f2(0),m2),col=3)

#f3
plot(x3,y3,type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)
m3 <- 1/3
lines(x3,t(x3,2,f3(2),m3),col=3)

#f4
plot(x4,y4,type='l',xlim=c(2.7,2.8),ylim=c(f4(2.75)-0.05,f4(2.75)+0.05))
points(2.75,f4(2.75),pch=16,col=2)
m4 <- 4
lines(x4,t(x4,2.75,f4(2.75),m4),col=3)